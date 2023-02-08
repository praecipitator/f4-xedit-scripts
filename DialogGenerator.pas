{
    Run on a quest, then select CSV to import the lines
    Made this for KG, don't remember which format the CSV must be
}
unit DialogGenerator;

//uses dubhFunctions;
uses praUtil;

const
    validEdidChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';
    cgString = 'Wait for Player Input CG:';
    combineLines = true;

var
    inputFile : string;
    targetQuest: IInterface;
    //dummyQuest, dummyScene: IInterface;

    // maps lineIDs and cgNrs to Phase Names, for lines which have been created already

    // caches what certain INFOs should point to.

    //charIdMap: TStringList;
    currentSceneNr: integer;

    dialogData: TJsonObject;
    createdObjectCache: TList;

    needsPostprocess: boolean;

function getFromCache(i: integer): IInterface;
begin
    Result := nil;
    if(i < createdObjectCache.count) then begin
        Result := ObjectToElement(createdObjectCache[i]);
    end;
end;

function getCacheId(e: IInterface): integer;
var
    i: integer;
    cur: IInterface;
begin
    Result := createdObjectCache.indexOf(e);
    if(Result < 0) then begin
        Result := createdObjectCache.add(e);
    end;
end;

function makeEditorId(prefix, str: string): string;
var
    i: integer;
    curChar: string;
begin
    //str := LowerCase(str);
    Result := prefix;

    for i:=1 to length(str) do begin
        curChar := copy(str, i, 1);
        if(pos(curChar, validEdidChars) > 0) then begin
            Result := Result + curChar;
        end;
    end;

    if(Result = prefix) then begin
        Result := prefix+'rand_'+IntToStr(random(10000));
        exit;
    end;


    if(length(Result) > 50) then begin
        Result := prefix + IntToHex(StringCRC32(str), 8);
    end;
end;

function LoadFromCsv(const bSorted, bDuplicates, bDelimited: Boolean; const d: String = ';'): TStringList;
var
	objFile: TOpenDialog;
	lsLines: TStringList;
begin
	lsLines := TStringList.Create;

	if bSorted then
		lsLines.Sorted;

	if bDuplicates then
		lsLines.Duplicates := dupIgnore;

	if bDelimited then
		if d <> '' then
			lsLines.NameValueSeparator := d
		else
			lsLines.NameValueSeparator := #44;

	objFile := TOpenDialog.Create(nil);

	try
		//objFile.InitialDir := GetCurrentDir;
		objFile.Options := [ofFileMustExist];
		objFile.Filter := '*.csv';
		objFile.FilterIndex := 1;
		if objFile.Execute then
			lsLines.LoadFromFile(objFile.FileName);
	finally
		objFile.Free;
	end;

	Result := lsLines;
end;

function getFirstOrCreate(parent: IInterface): IInterface;
begin
    if(ElementCount(parent) = 0) then begin
        Result := ElementAssign(parent, HighInteger, nil, False);
        exit;
    end;

    Result := ElementByIndex(parent, 0);
end;

function explodeStr(str: string; delimiter: string): TSTringList;
var
    fields: TStringList;
begin
    fields := TStringList.Create;

    fields.Delimiter := delimiter;
    fields.StrictDelimiter := TRUE;
    fields.DelimitedText := str;


    Result := fields;
end;

function getCharName(notes: String): String;
var
    i: integer;
    parts: TStringList;
begin
    if(notes = '') then begin
        Result := '';
        exit;
    end;
    parts := explodeStr(notes, '|');

    if(parts.count < 2) then begin
        Result := '';
        exit;
    end;

    Result := Trim(parts[1]);

    parts.free;
end;

function getCharAliasId(charName: string): integer;
var
    aliases, curAlias: IInterface;
    i, curId, highestId: integer;
    curName, charNameLowercased: string;
begin
    // player is hardcoded to -2??
    if(charName = '') then begin
        Result := -2;
        exit;
    end;


    // AddMessage('getCharAliasId for '+charName);
    Result := -1;
    highestId := -1;
    aliases := ElementByPath(targetQuest, 'Aliases');
    if(not assigned(aliases)) then begin
        aliases := Add(targetQuest, 'Aliases', true);
        if(ElementCount(aliases) > 0)  then begin
            curAlias := ElementByIndex(aliases, 0);
            Result := 0;

            //curAlias := ElementAssign(aliases, HighInteger, nil, False);
            SetElementEditValues(curAlias, 'ALID', charName);
            SetElementEditValues(curAlias, 'ALST', '0');

            SetElementEditValues(targetQuest, 'ANAM', '1');
            exit;
        end;
    end;

    charNameLowercased := LowerCase(charName);

    for i:=0 to ElementCount(aliases)-1 do begin
        curAlias := ElementByIndex(aliases, i);
        curName := LowerCase(trim(GetElementEditValues(curAlias, 'ALID')));
        curId := StrToInt(GetElementEditValues(curAlias, 'ALST'));

        if(curId > highestId) then begin
            highestId := curId;
        end;

        if(curName = charNameLowercased) then begin
            Result := curId;
            exit;
        end;
    end;

    // if not, add
    Result := highestId+1;

    curAlias := ElementAssign(aliases, HighInteger, nil, False);
    SetElementEditValues(curAlias, 'ALID', charName);
    SetElementEditValues(curAlias, 'ALST', IntToStr(Result));

    SetElementEditValues(targetQuest, 'ANAM', IntToStr(Result+1));

end;

procedure ensureSceneHasActor(scene: IInterface; actorAliasID: integer; prepend: boolean);
var
    sceneActors, curSceneActor, dnam: IInterface;
    i, curIndex: integer;
begin
    // AddMessage('Adding actor '+IntToStr(actorAliasID));
    sceneActors := ElementByPath(scene, 'Actors');
    if(assigned(sceneActors)) then begin
        // AddMessage('Yes have actors');
        for i:=0 to ElementCount(sceneActors)-1 do begin
            curSceneActor := ElementByIndex(sceneActors, i);
            curIndex := StrToInt(GetElementEditValues(curSceneActor, 'ALID'));
            if(curIndex = actorAliasID) then begin
                // AddMessage('Actor exists already');
                exit;
            end;
        end;
        curSceneActor := nil;
    end else begin
        // AddMessage('No have no actors');
        sceneActors := Add(scene, 'Actors', true);
        if(ElementCount(sceneActors) > 0) then begin
            // AddMessage('Got the free actor');
            curSceneActor := ElementByIndex(sceneActors, 0);
        end;
    end;

    if(not assigned(curSceneActor)) then begin
        // AddMessage('Creating new actor');
        curSceneActor := ElementAssign(sceneActors, HighInteger, nil, False);
    end;

    SetElementEditValues(curSceneActor, 'ALID', IntToStr(actorAliasID));

    if(actorAliasID <> -2) then begin
        SetElementEditValues(curSceneActor, 'LNAM', '001');
    end;

    dnam := EnsurePath(curSceneActor, 'DNAM');
    SetElementEditValues(dnam, 'Death End', '1');
    SetElementEditValues(dnam, 'Combat End', '1');
    SetElementEditValues(dnam, 'Dialogue Pause', '1');


end;

function addResponseToInfo(info: IInterface; response: string): IInterface;
var
    rspRoot, curRsp: IInterface;
begin
    rspRoot := ElementByPath(info, 'Responses');
    
    if(not assigned(rspRoot)) then begin
        rspRoot := EnsurePath(info, 'Responses');
        curRsp := getFirstOrCreate(rspRoot);
    end else begin
        curRsp := ElementAssign(rspRoot, HighInteger, nil, False);        
    end;
    
    SetElementEditValues(curRsp, 'NAM1', response);

    Result := curRsp;
end;


function createDialogTopic(quest: IInterface; subtype, subtypeName: string): IInterface;
var
    dialData, maybeDial, questGroup: IInterface;
	useEdID: string;
    i: integer;
begin

	if(subtypeName = 'GREE') then begin
		// Check for existing greeting type
        questGroup := ChildGroup(targetQuest);
        for i:=0 to ElementCount(questGroup)-1 do begin
            maybeDial:= ElementByIndex(questGroup, i);
            if(GetElementEditValues(maybeDial, 'SNAM') = 'GREE') then begin
                AddMessage('found existing GREE');
                Result := maybeDial;
                exit;
            end;
        end;
	end;


	Result := Add(quest, 'DIAL', true);
	dialData := EnsurePath(Result, 'DATA');
	SetLinksTo(EnsurePath(Result, 'QNAM'), quest);

	SetElementEditValues(dialData, 'Category', 'Player');
	SetElementEditValues(dialData, 'Subtype', subtype);

	SetElementEditValues(Result, 'SNAM', subtypeName);

	if(useEdID <> '') then begin
		SetElementEditValues(Result, 'EDID', useEdID);
	end;
end;


function getEmotionKeyword(emotion: string): IInterface;
var
    edid: string;
begin
    Result := nil;

    if(emotion = '') then exit;

    edid := 'AnimFaceArchetype'+strUpperCaseFirst(emotion);

    // AddMessage('Emotion KW is '+edid);
    Result := FindObjectByEdid(edid);

end;

procedure addInfoResponse(info: IInterface; text, emotion: string);
var
    trda, rsp, emotionKw: IInterface;
begin
    rsp := addResponseToInfo(info, text);

    // add this
    Add(info, 'ENAM', true);

    // set the other stuff
    trda := EnsurePath(rsp, 'TRDA');

    emotionKw := getEmotionKeyword(emotion);

    if(not assigned(emotionKw)) then begin
        SetElementEditValues(trda, 'Emotion', 'FFFF - None Reference [FFFFFFFF]');
    end else begin
        setPathLinksTo(trda, 'Emotion', emotionKw);
    end;
    SetElementEditValues(trda, 'Response Number', '1');
    SetElementEditValues(trda, 'Camera Target Alias', '-1');
    SetElementEditValues(trda, 'Camera Location Alias', '-1');
end;

function createDialogInfo(dialog: IInterface; text, emotion: string): IInterface;
var
    trda, rsp, emotionKw: IInterface;
    numInfosStr: string;
    numInfos: integer;
begin
    Result := Add(dialog, 'INFO', true);
    addInfoResponse(Result, text, emotion);
{    
    rsp := addResponseToInfo(Result, text);

    // add this
    Add(Result, 'ENAM', true);

    // set the other stuff
    trda := EnsurePath(rsp, 'TRDA');

    emotionKw := getEmotionKeyword(emotion);

    if(not assigned(emotionKw)) then begin
        SetElementEditValues(trda, 'Emotion', 'FFFF - None Reference [FFFFFFFF]');
    end else begin
        setPathLinksTo(trda, 'Emotion', emotionKw);
    end;
    SetElementEditValues(trda, 'Response Number', '1');
    SetElementEditValues(trda, 'Camera Target Alias', '-1');
    SetElementEditValues(trda, 'Camera Location Alias', '-1');
}
    if(EditorId(dialog) = '') then begin
        SetElementEditValues(dialog, 'EDID', makeEditorId(EditorId(targetQuest) + '_', text));
    end;

    numInfosStr := GetElementEditValues(dialog, 'TIFC');
    if(numInfosStr = '') then begin
        numInfos := 0;
    end else begin
        numInfos := StrToInt(numInfosStr);
    end;

    SetElementEditValues(dialog, 'TIFC', IntToStr(numInfos+1));

end;

function makeGreeting(text: string; aliasIndex: integer; targetScene: IInterface; emotion: string): IInterface;
var
    dial, info, dialData, conditionsRoot, newCondition, ctda: IInterface;
    i: integer;
begin
    // ensure it exists in the scene, too
    ensureSceneHasActor(targetScene, aliasIndex, true);

    dial := createDialogTopic(targetQuest, 'Greeting', 'GREE');


    info := createDialogInfo(dial, text, emotion);

    // condition
    conditionsRoot := EnsurePath(info, 'Conditions');
    newCondition := getFirstOrCreate(conditionsRoot);
    ctda := EnsurePath(newCondition, 'CTDA');
    //SetElementEditValues(ctda, 'Type', 'Equal to');
    SetElementEditValues(ctda, 'Comparison Value', '1.0');
    //SetElementEditValues(ctda, 'Type', 'equal');
    SetElementEditValues(ctda, 'Function', 'GetIsAliasRef');
    //SetElementEditValues(ctda, 'Type', 'equal to');
    SetElementEditValues(ctda, 'Alias', IntToStr(aliasIndex));


    // Type=00000000 = not equals
    // Type=10000000 = equals
    SetElementEditValues(ctda, 'Type', '10000000');// goddammit stick!

	SetLinksTo(EnsurePath(info, 'TSCE'), targetScene);

    // AddMessage('Made greeting '+text);
    Result := info;
end;

procedure putIntoList(index: integer; obj: variant; list: TList);
var
    i: integer;
begin

    if(index >= list.count) then begin
        for i := list.count to index do begin
            list.add(nil);
        end;
    end;

    list[index] := obj;
end;

procedure putIntoStringList(index: integer; obj: string; list: TStringList);
var
    i: integer;
begin

    if(index >= list.count) then begin
        for i := list.count to index do begin
            list.add(nil);
        end;
    end;

    list[index] := obj;
end;

function getStringFromList(index: integer; list: TStringList): string;
begin
    Result := '';
    if(list.count <= index) then begin
        exit;
    end;

    //AddMessage('listcount '+IntToStr(list.count)+', index '+IntToStr(index));
    if(not assigned(list[index])) then begin
        exit;
    end;

    if(length(list[index]) = 0) then begin
        exit;
    end;

    Result := list[index];
end;


function findGreetingLine(): integer;
var
    i, gotoId: integer;
    lines, curLine: TJsonObject;
    curName: string;
begin
    Result := -1;
    // find something which looks like a greeting
    // it should have no player prompt, no choice group, and a go to line != own line ID
	// KG Note: Testing for just the above. I think the test would be to take a line, and confirm that no other line has a goto pointing to it, which means it has to be the first line in a scene.
    lines := dialogData.O['Lines'];

    for i:=0 to lines.count-1 do begin
		curName := lines.names[i];
        curLine := lines.O[curName];

		if(curLine.S['playerPrompt'] = '') and (curLine.S['type'] <> 'cg') and (curLine.O['goTo'].count > 0) and (curLine.O['goTo'].I['index'] <> i) then begin
			AddMessage('Line ' + curName + ' appears to be a greeting line.');
            Result := StrToInt(curName);
            exit;
        end;
    end;
end;

function createInScene(name: string; targetScene: IInterface): IInterface;
var
    sourceRoot, targetRoot, dummy: IInterface;
begin
    if(not ElementExists(targetScene, name)) then begin
        targetRoot := Add(targetScene, name, true);

        if(ElementCount(targetRoot) > 0) then begin
            Result := ElementByIndex(targetRoot, 0);
            exit;
        end;
    end else begin
        targetRoot := ElementByPath(targetScene, name);
    end;

    Result := ElementAssign(targetRoot, HighInteger, nil, false);
end;

function getPhaseName(phaseNr: integer): string;
begin
    Result := 'Phase_'+IntToStr(phaseNr+1);
end;

function createPhase(scene: IInterface): integer;
var
    phaseRoot, newPhase: IInterface;
begin
    newPhase := createInScene('Phases', scene);
    // le workaround
    ElementAssign(newPhase, 0, nil, False); // HNAM - Marker Phase Start
    ElementAssign(newPhase, 3, nil, False); // NEXT - Marker Start Conditions
    ElementAssign(newPhase, 5, nil, False); // NEXT - Marker Completion Conditions
    ElementAssign(newPhase, 9, nil, False); // HNAM - Marker Phase End


    phaseRoot := EnsurePath(scene, 'Phases');

    // SetToDefault(newPhase);

    SetElementEditValues(newPhase, 'WNAM', '350');

    //SetElementEditValues(newPhase, 'HNAM - Marker Phase Start', '');

    //SetElementEditValues(newPhase, 'NEXT - Marker Start Conditions', '');

    Result := ElementCount(phaseRoot)-1;

    SetElementEditValues(newPhase, 'NAM0 - Name', getPhaseName(Result));


end;

function createAction(scene: IInterface; phaseNr: integer; actionType: string; aliasId: integer): IInterface;
var
    actionRoot, newAction, fuqAction, curAction: IInterface;
    lastIndex, i, curIndex: integer;
    curIndexStr: string;
begin
    newAction := createInScene('Actions', scene);

    // le workaround
    ElementAssign(newAction, 42, nil, False); // ANAM - End Marker

    lastIndex := 0;

    actionRoot := ElementByPath(scene, 'Actions');

    for i:=0 to ElementCount(actionRoot)-1 do begin
        curAction := ElementByIndex(actionRoot, i);
        curIndexStr := GetElementEditValues(curAction, 'INAM');
        if (curIndexStr <> '') then begin
            curIndex := StrToInt(curIndexStr);
            if(curIndex > lastIndex) then begin
                lastIndex := curIndex;
            end;
        end;
    end;


    SetElementEditValues(newAction, 'INAM', IntToStr(lastIndex+1));
    SetElementEditValues(newAction, 'SNAM', IntToStr(phaseNr));
    SetElementEditValues(newAction, 'ENAM', IntToStr(phaseNr));

    // other stuff
    SetElementEditValues(newAction, 'ANAM - Type', actionType);
    SetElementEditValues(newAction, 'NAM0 - Name', '');
    SetElementEditValues(newAction, 'ALID - Alias ID', IntToStr(aliasId));

    SetElementEditValues(scene, 'INAM', IntToStr(lastIndex+1));

    Result := newAction;
end;

function getTypeFromPrompt(promptStr: string): string;
var
    curChar: string;
    i: integer;
begin
    Result := '';
    // type is like: ML1-Y-r1
    // valid type letters are YBAX
    for i:=1 to length(promptStr) do begin
        curChar := copy(promptStr, i, 1);
        if (curChar = 'Y') or (curChar = 'B') or (curChar = 'A') or (curChar = 'X') then begin
            Result := curChar;
            exit;
        end;
    end;
end;

procedure dumpStringList(a: TStringList);
var
    i: integer;
begin
    AddMessage('== DUMPING STRINGLIST ==');
    for i:=0 to a.count-1 do begin
        if(a[i] = nil) then begin
            AddMessage(IntToStr(i)+' IS NIL');
        end else begin
            AddMessage(IntToStr(i)+' -> '+a[i]);
        end;
    end;
end;

procedure addLineToChoiceGroup(line: TJsonObject; scene: IInterface; action: IInterface);
var
    i, choiceGroupNr: integer;
    typeStr, playerLine, npcLine, nextLineStr: string;
    playerDial, npcDial, playerInfo, npcInfo, infoToLink, pathCheck: IInterface;
begin
    // find the type
    typeStr := getTypeFromPrompt(line.S['playerPrompt']);
    if(typeStr = '') then begin
        AddMessage('Got no typestring for a choice line');
        exit;
    end;

   // dumpStringList(line);

    playerLine := line.S['playerLine'];
    npcLine := line.S['npcLine'];

    // create stuff
    // player
    playerDial := createDialogTopic(targetQuest, 'Custom', 'SCEN');
    playerInfo := createDialogInfo(playerDial, playerLine, line.S['playerEmotion']);

    // NPC
    npcDial := createDialogTopic(targetQuest, 'Custom', 'SCEN');
    // the info might be missing.
    if(npcLine <> '') then begin
        npcInfo := createDialogInfo(npcDial, npcLine, line.S['npcEmotion']);
    end;

	AddMessage('Attempting to configure choice group...');

    // A = positive, B = negative, X = neutral, Y = question
    if(typeStr = 'A') then begin
		SetLinksTo(EnsurePath(action, 'PTOP'), playerDial);// player positive
        SetLinksTo(EnsurePath(action, 'NPOT'), npcDial);// npc positive
    end else if(typeStr = 'B') then begin
		SetLinksTo(EnsurePath(action, 'NTOP'), playerDial);// player negative
        SetLinksTo(EnsurePath(action, 'NNGT'), npcDial);// npc negative
    end else if(typeStr = 'X') then begin
		SetLinksTo(EnsurePath(action, 'NETO'), playerDial);// player neutral
        SetLinksTo(EnsurePath(action, 'NNUT'), npcDial);// npc neutral
    end else if(typeStr = 'Y') then begin
		SetLinksTo(EnsurePath(action, 'QTOP'), playerDial);// player question
		SetLinksTo(EnsurePath(action, 'NQUT'), npcDial);// npc question
    end;

    // which to link?
    infoToLink := playerInfo;
    if(assigned(npcInfo)) then begin
        infoToLink := npcInfo;
    end;


    //goToData := lineData.O['goToData'];

    if(line.O['goTo'].count < 0) then begin
        SetElementEditValues(EnsurePath(info, 'ENAM\Flags'), 'End Running Scene', 1);
        exit;
    end;

    // don't link the other stuff yet
    line.I['InfoToLink'] := createdObjectCache.add(infoToLink);
    needsPostprocess := true;
end;


procedure linkToExistingPhase(info: IInterface; scene: IInterface; phase: string);
begin
    if(phase = '') then begin
        exit;
    end;

	AddMessage('Trying to link '+ Name(info) +' to '+phase);

    ElementAssign(info, 12, nil, False);
    SetElementEditValues(info, 'TSCE - Start Scene', IntToHex(GetLoadOrderFormID(scene), 8));
    if(phase <> '') then begin
        ElementAssign(info, 18, nil, False);
        SetElementEditValues(info, 'NAM0 - Start Scene Phase', phase);
    end;

    // fuuu
    // 4 = pnam - Previous INFO
    // 5 = dnam - shared info
    // 6 = gnam - info group
    // 7 = IOVR - Override File Name
    // 8 is probably responses
    // 9 = Conditions
    //10 = RNAM - prompt
    //11 = ANAM - Speaker
    //12 = TSCE - Start Scene <- FINALLY
    //13 = INTV - Unknown
    //14 = ALFA - Forced Alias
    //15 = ONAM - Audio Output Override
    //16 = GREE - Greet Distance
    //17 = TIQS - Set Parent Quest Stage
    //18 = NAM0 - Start Scene Phase <- FINALLY
    //19 = INCC - Challenge
    //ElementAssign(info, 4, nil, False);
    //ElementAssign(info, 5, nil, False);
end;

function getChoiceGroupNr(nextLineStr: string): integer;
begin
    Result := -1;

    if(strStartsWith(nextLineStr, cgString)) then begin
        Result := StrToInt(Trim(copy(nextLineStr, length(cgString)+1, 255)));
    end;

end;

procedure linkGreeting(infoToLink, scene: IInterface; targetPhase: string);
var
    i: integer;
    phaseRoot, curPhase: IInterface;
    firstPhaseName: string;
begin
    // always link it to the scene, but only use the name if it's not the first
    phaseRoot := EnsurePath(scene, 'Phases');
    firstPhaseName := '';
    if(ElementCount(phaseRoot) > 0) then begin
        curPhase := ElementByIndex(phaseRoot, 0);
        firstPhaseName := GetElementEditValues(curPhase, 'NAM0 - Name');
        if(firstPhaseName = targetPhase) then begin
            targetPhase := '';
        end;
    end;
    linkToExistingPhase(infoToLink, scene, targetPhase);
end;

procedure linkInfoIfNecessary(infoToLink, scene: IInterface; phaseFrom, phaseTo: string);
var
    i: integer;
    phaseRoot, curPhase: IInterface;
    curName, prevName: string;
begin
    // link infoToLink to phaseTo, but NOT if phaseTo comes right after phaseFrom
    phaseRoot := EnsurePath(scene, 'Phases');

    for i:=0 to ElementCount(phaseRoot)-1 do begin
        curPhase := ElementByIndex(phaseRoot, i);
        if(i = 0) then begin
            prevName := GetElementEditValues(curPhase, 'NAM0 - Name');
        end else begin
            curName := GetElementEditValues(curPhase, 'NAM0 - Name');

            if ((prevName = phaseFrom) and (curName = phaseTo)) then begin
                // found!
                AddMessage('Phases '+phaseFrom+' and '+phaseTo+' don''t need linking');
                exit;
            end;

            prevName := curName;
        end;
    end;

    // if still alive, this needs linking
    AddMessage('Phases '+phaseFrom+' and '+phaseTo+' DO need linking');
    linkToExistingPhase(infoToLink, scene, phaseTo);
end;

function createDialogLine(index: integer; scene: IInterface): TJsonObject;
var
    lineData, goToData, nextPhaseData: TJsonObject;
    aliasIndex, phaseNr: integer;
    action, dial, info, nextScene: IInterface;
    nextPhaseName, myPhaseName, curNpcName: string;
begin
    Result := nil;
    lineData := dialogData.O['Lines'].O[index];
    if(lineData.S['Phase'] <> '') then begin
        Result := TJsonObject.create;
        Result.S['Phase'] := lineData.S['Phase'];
        Result.I['Scene'] := lineData.I['Scene'];
        exit;
    end;

    // other stuff
    if(lineData.B['isChoiceGroup']) then begin
        // redirect to CG
        Result := createDialogChoiceGroup(lineData.I['choiceGroupId'], scene);
        exit;
    end;

    if(lineData.count = 0) then begin
        AddMessage('Found no line for index '+IntToStr(index));
        exit;
    end;

    AddMessage('Creating Dialog Line for '+lineData.toJson());

    curNpcName := lineData.S['npcName'];

    if(curNpcName = '') then begin
        aliasIndex := -2; // is this hardcoded?
    end else begin
        aliasIndex := getCharAliasId(curNpcName);
    end;
    ensureSceneHasActor(scene, aliasIndex, false);

    // create a dialog, and a choice group, if one exists
    phaseNr := createPhase(scene);
    action := createAction(scene, phaseNr, 'Dialogue', aliasIndex);

    dial := createDialogTopic(targetQuest, 'Custom', 'SCEN');
    info := createDialogInfo(dial, lineData.S['npcLine'], lineData.S['npcEmotion']);
	//AddMessage('SetLinksTo called for CreateDialogueLine Data');
	SetLinksTo(EnsurePath(action, 'DATA'), dial);
    // no idea what this is
    SetElementEditValues(action, 'DMAX', '10.0');
    SetElementEditValues(action, 'DMIN', '1.0');

    myPhaseName := getPhaseName(phaseNr);
    lineData.S['Phase'] := myPhaseName;
    lineData.I['Scene'] := getCacheId(scene);

    Result := TJsonObject.create;
    Result.S['Phase'] := lineData.S['Phase'];
    Result.I['Scene'] := lineData.I['Scene'];
    //

    goToData := lineData.O['goTo'];

    if(goToData.count > 0) then begin
        linkOrAppendNext(goToData, scene, info, dial, myPhaseName, curNpcName);
        {
        nextPhaseData := createNext(goToData, scene);
        nextPhaseName := nextPhaseData.S['Phase'];
        nextScene := getFromCache(nextPhaseData.I['Scene']);
        nextPhaseData.free();
        //nextPhaseName := createNext(goToData, scene);
        linkInfoIfNecessary(info, nextScene, myPhaseName, nextPhaseName);

        }
    end;
end;

function createDialogChoiceGroup(index: IInterface; scene: IInterface): TJsonObject;
var
    phaseNr, i, choiceGroup, aliasIndex: integer;
    action: IInterface;
    //curData : TJsonArray;
    curLine, curData: TJsonObject;
    choiceGroupStr, maybeAliasIndex, myPhaseName, fallbackNpcName, npcName: string;
begin
    curData := dialogData.O['ChoiceGroups'].O[index];

    if(curData.S['Phase'] <> '') then begin
        Result := TJsonObject.create;
        Result.S['Phase'] := curData.S['Phase'];
        Result.I['Scene'] := curData.I['Scene'];
        exit;
    end;

    fallbackNpcName := curData.S['fallbackNpcName'];

    // first, I need another phase and another action
    phaseNr := createPhase(scene);
    aliasIndex := -1; //temp
    action := createAction(scene, phaseNr, 'Player Dialogue', aliasIndex);
    SetElementEditValues(action, 'DTGT - Dialogue Target Actor', aliasIndex);

    for i:=0 to curData.A['lines'].count-1 do begin
        curLine := curData.A['lines'].O[i];

        addLineToChoiceGroup(curLine, scene, action);

        npcName := curLine.S['npcName'];

        if(npcName = '') then begin
            npcName := fallbackNpcName;
        end;

        if(npcName <> '') then begin
            if(aliasIndex < 0) then begin
                aliasIndex := getCharAliasId(npcName);
                if(aliasIndex >= 0) then begin
                    ensureSceneHasActor(scene, aliasIndex, false);
                    SetElementEditValues(action, 'ALID - Alias ID', IntToStr(aliasIndex));
                    SetElementEditValues(action, 'DTGT - Dialogue Target Actor', aliasIndex);
                end;
            end;
        end;
    end;

    myPhaseName := getPhaseName(phaseNr);
    curData.S['Phase'] := myPhaseName;
    curData.I['Scene'] := getCacheId(scene);

    Result := TJsonObject.create;
    Result.S['Phase'] := curData.S['Phase'];
    Result.I['Scene'] := curData.I['Scene'];
end;

function createNext(goToData: TJsonObject; scene: IInterface): TJSONObject;
var
    index: integer;
    phaseName: string;
begin
    Result := nil;
    phaseName := '';

    index := goToData.I['index'];
    if(goToData.S['type'] = 'line') then begin
        // create normal line
        Result := createDialogLine(index, scene);
    end else if(goToData.S['type'] = 'cg') then begin
        // create choice group
        Result := createDialogChoiceGroup(index, scene);
    end;

    if(Result <> nil) then begin
        AddMessage('Created phase '+Result.toJson());
    end else begin
        AddMessage('Failed to create stuff for '+goToData.toJSON());
    end;
end;

procedure linkOrAppendNext(goToData: TJsonObject; scene, info, dial: IInterface; myPhaseName, curNpcName: string);
var
    lineData, nextPhaseData: TJsonObject;
    defaultBehavior: boolean;
    index: integer;
    nextPhaseName, nextNpcName: string;
    nextScene, newInfo: IInterface;
begin
    // HERE
    // default behavior if:
    // - goto is choice group
    // - goto is line, but has hasJumpsTo=true
    // - speaker at goto differs from current speaker
    if(goToData.count <= 0) then begin
        exit;
    end;

    defaultBehavior := true;
    if(goToData.S['type'] = 'cg') then begin
        defaultBehavior := true;
    end else if (goToData.S['type'] = 'line') then begin
        index := goToData.I['index'];
        lineData := dialogData.O['Lines'].O[index];
        if(lineData.B['hasJumpsTo'] <> true) then begin

            //
            if(lineData.S['npcName'] = curNpcName) then begin
                defaultBehavior := false;
            end;
        end;
    end;

    AddMessage('Is defaultBehavior? '+BoolToStr(defaultBehavior));

    if(defaultBehavior) then begin
        nextPhaseData := createNext(goToData, scene);
        nextPhaseName := nextPhaseData.S['Phase'];
        nextScene := getFromCache(nextPhaseData.I['Scene']);
        nextPhaseData.free();
        linkInfoIfNecessary(info, nextScene, myPhaseName, nextPhaseName);
        exit;
    end;

    // instead of making a new info, only make a new Response, and add it to the existing info
    addInfoResponse(info, lineData.S['npcLine'], lineData.S['npcEmotion']);
    linkOrAppendNext(lineData.O['goTo'], scene, info, dial, myPhaseName, curNpcName);
    
    // // now instead make a new info, and append it to existing DIAL
    // newInfo := createDialogInfo(dial, lineData.S['npcLine'], lineData.S['npcEmotion']);
    // linkOrAppendNext(lineData.O['goTo'], scene, newInfo, dial, myPhaseName, curNpcName);
end;

procedure postprocessChoiceGroup(cg: TJsonObject; scene: IInterface);
var
    lines: TJsonArray;
    i, infoToLink: integer;
    curLine, nextData: TJsonObject;
    info, nextScene: IInterface;
    nextPhase: string;
begin
    lines := cg.A['lines'];

    for i:=0 to lines.count-1 do begin
        curLine := lines.O[i];
        infoToLink := curLine.I['InfoToLink'];
        if(infoToLink >= 0) then begin
            info := ObjectToElement(createdObjectCache[infoToLink]);

            if(curLine.O['goTo'].count > 0) then begin

                nextData := createNext(curLine.O['goTo'], scene);
                nextPhase := nextData.S['Phase'];
                nextScene := getFromCache(nextData.I['Scene']);
                nextData.free();

                AddMessage('Linking '+Name(info)+' to '+nextPhase);

                SetElementEditValues(EnsurePath(info, 'ENAM\Flags'), 'Start Scene on End', 1);
                //SetElementEditValues(info, 'ENAM\Flags\Start Scene on End', '1');
                linkToExistingPhase(info, nextScene, nextPhase);
            end else begin
                AddMessage('Postprocessing CG has no goTo: '+curLine.toJSON());
                SetElementEditValues(EnsurePath(info, 'ENAM\Flags'), 'End Running Scene', 1);
                //SetElementEditValues(info, 'ENAM\Flags\End Running Scene', '1');
            end;

            //AddMessage('Would postprocess forID '+IntToStr(infoToLink)+' -> '+curLine.toJSON());
        end;
    end;
end;

function getNextSceneData(goToData: TJsonObject): TJsonObject;
var
    index: integer;
    curData: TJsonObject;
begin
    Result := nil;
    if(goToData.count <= 0) then begin
        exit;
    end;

    index := goToData.I['index'];

    if(goToData.S['type'] = 'line') then begin
        curData := dialogData.O['Lines'].O[index];
        if(curData.S['Phase'] <> '') then begin
            Result := TJsonObject.create;
            Result.S['Phase'] := curData.S['Phase'];
            Result.I['Scene'] := curData.I['Scene'];
            exit;
        end;
    end else if(goToData.S['type'] = 'cg') then begin
        curData := dialogData.O['ChoiceGroups'].O[index];

        if(curData.S['Phase'] <> '') then begin
            Result := TJsonObject.create;
            Result.S['Phase'] := curData.S['Phase'];
            Result.I['Scene'] := curData.I['Scene'];
            exit;
        end;
    end;
end;

procedure createGreeting(greetIndex: integer);
var
    i, nextLineIndex, aliasIndex: integer;
    currentScene, sceneFlags, currentGreeting: IInterface;
    greetingLine, goToData, nextData, curLine: TJsonObject;
    greetAliasName, linkData, linkPhaseName, curName, edidSuffix: string;
begin
    needsPostprocess := false;
    AddMessage('Generating greeting for line '+IntToStr(greetIndex));

    greetingLine := dialogData.O['Lines'].O[greetIndex];

    greetAliasName := greetingLine.S['npcName'];
    aliasIndex := getCharAliasId(greetAliasName);
    goToData := greetingLine.O['goTo'];
    // see if the scene already exists
    nextData := getNextSceneData(goToData);

    //if whatever comes next hasn't been done yet, make a new scene
    if(nextData = nil) then begin
        AddMessage('Scene doesn''t exist yet, creating');
        currentScene := Add(targetQuest, 'SCEN', true);


        if(currentSceneNr < 10) then begin
            edidSuffix := '0'+IntToStr(currentSceneNr);
        end else begin
            edidSuffix := IntToStr(currentSceneNr);
        end;
        //AddMessage('Suffix >'+edidSuffix+'<');
        currentSceneNr := currentSceneNr + 1;

        //SetElementEditValues(currentScene, 'EDID', makeEditorId(EditorId(targetQuest) + '_', greetingLine.S['edidBase']));
        SetElementEditValues(currentScene, 'EDID', makeEditorId(EditorId(targetQuest) + '_', edidSuffix));
        SetLinksTo(EnsurePath(currentScene, 'PNAM'), targetQuest);

        // add a lot of default stuff to the scene
        sceneFlags := Add(currentScene, 'FNAM', true);

        SetElementEditValues(sceneFlags, 'Unknown 2', '1');
        SetElementEditValues(sceneFlags, 'Unknown 5', '1');
        // and another hack
        ElementAssign(currentScene, 18, nil, False); // XNAM - Index
        //SetElementEditValues(currentScene, 'XNAM - Index', '0'); // this is 0 for all of them
        linkPhaseName := '';
    end else begin
        currentScene := getFromCache(nextData.I['Scene']);
        linkPhaseName := nextData.S['Phase'];
        AddMessage('Scene exists already, will use '+Name(currentScene));
        nextData.free();
    end;

    currentGreeting := makeGreeting(greetingLine.S['npcLine'], aliasIndex, currentScene, greetingLine.S['npcEmotion']);

    if(goToData.count = 0) then begin
        AddMessage('Greeting has no next line?');
        exit;
    end;

    // this should now create a lot of stuff, following the next lines
    nextData := createNext(goToData, currentScene);
    currentScene := getFromCache(nextData.I['Scene']);
    linkGreeting(currentGreeting, currentScene, nextData.S['Phase']);
    nextData.free();

    while needsPostprocess do begin
        AddMessage('Postprocessing Choice Group');
        needsPostprocess := false; // postprocessing might set it back to true
        for i:=0 to dialogData.O['ChoiceGroups'].count-1 do begin
            curName := dialogData.O['ChoiceGroups'].names[i];
            curLine := dialogData.O['ChoiceGroups'].O[curName];

            if(curLine.S['Phase'] <> '') then begin
                // only postprocess CGs which have been processed and therefore have a Phase
                postprocessChoiceGroup(curLine, currentScene);
            end;
        end;
    end;

    postprocessScene(currentScene);
end;

procedure postprocessScene(scene: IInterface);
var
    actors, actions, curActorEntry, curActionEntry, htid, firstHtid: IInterface;
    i, numActors: integer;
    hasPlayer, isFirstLine: boolean;
    curActorId, otherActorId: string;
begin
    actors := ElementByPath(scene, 'Actors');
    actions := ElementByPath(scene, 'Actions');

    numActors := ElementCount(actors);
    if(numActors <> 2) then begin
        exit;
    end;


    AddMessage('postprocessing scene');
    hasPlayer := false;

    for i:=0 to numActors-1 do begin
        curActorEntry := ElementByIndex(actors, i);
        curActorId := GetElementEditValues(curActorEntry, 'ALID');
        if(curActorId = '-2') then begin
            //AddMessage('yes player');
            hasPlayer := true;
        end else begin
            otherActorId := curActorId;
        end;
    end;


    isFirstLine := true;
    for i:=0 to ElementCount(actions)-1 do begin
        curActionEntry := ElementByIndex(actions, i);
        if(GetElementEditValues(curActionEntry, 'ANAM') = 'Dialogue') then begin
            curActorId := GetElementEditValues(curActionEntry, 'ALID');

            if(curActorId <> '-2') then begin
                if(isFirstLine) then begin
                    isFirstLine := false;
                    SetElementEditValues(curActionEntry, 'FNAM', '000000000000100101');
                end else begin
                    SetElementEditValues(curActionEntry, 'FNAM', '000000000000100001');
                end;
            end else begin
                // htid := EnsurePath(curActionEntry, 'HTID - Player Headtracking');
                // oh god, this again...
                htid := ElementAssign(curActionEntry, 38, nil, False);
                firstHtid := ElementByIndex(htid, 0);
                SetEditValue(firstHtid, otherActorId);
            end;
        end;
    end;
end;

procedure createScenes();
var
    i, greetLine: integer;
    greetings: TJsonArray;
begin
    greetings := dialogData.A['Greetings'];

    for i:=0 to greetings.count-1 do begin
       greetLine := greetings.I[i];

       createGreeting(greetLine);
    end;
end;

procedure clearCaches();
begin
    needsPostprocess := false;
end;

procedure setGoToData(parentObj: TJSONObject; goToLine, goToChoiceGroup: integer; lastSpeakerName: string);
var
    currentGoToData, currentChoiceGroup: TJsonObj;
begin
    if(goToLine >= 0) or (goToChoiceGroup >= 0) then begin
        currentGoToData := parentObj.O['goTo'];
        //currentChoiceGroup.O['npcName']
        if(goToLine >= 0) then begin
            currentGoToData.S['type'] := 'line';
            currentGoToData.I['index'] := goToLine;
        end else begin
            currentGoToData.S['type'] := 'cg';
            currentGoToData.I['index'] := goToChoiceGroup;

            if(lastSpeakerName <> '') then begin
                // HACK
                currentChoiceGroup := dialogData.O['ChoiceGroups'].O[goToChoiceGroup];
                currentChoiceGroup.S['fallbackNpcName'] := lastSpeakerName;
            end;
        end;
        //currentGoToData.I['IInterface'] := -1; // index in createdObjectCache
        //currentGoToData.S['Phase'] := ''; // index in createdObjectCache
    end;
end;

procedure addToChoiceGroup(cgId: integer; playerPrompt, playerLine, npcLine, npcName: string; goToLine, goToChoiceGroup, lineId: integer; playerEmotion, npcEmotion: string);
var
    choiceGroups, currentChoiceGroup, currentLine, currentGoToData: TJSONObject;
begin
    choiceGroups := dialogData.O['ChoiceGroups'];

    currentChoiceGroup := choiceGroups.O[cgId];

    currentChoiceGroup.S['Phase'] := '';
    currentChoiceGroup.I['Scene'] := -1;
    currentLine := currentChoiceGroup.A['lines'].addObject();

    currentLine.S['playerPrompt'] := playerPrompt;
    currentLine.S['playerLine'] := trim(playerLine);
    currentLine.S['npcLine'] := trim(npcLine);
    currentLine.S['npcName'] := trim(npcName);
    currentLine.S['edidBase'] := playerLine;
    currentLine.I['InfoToLink'] := -1; //createdObjectCache
    currentLine.S['npcEmotion'] := npcEmotion;
    currentLine.S['playerEmotion'] := playerEmotion;
    currentLine.B['hasJumpsTo'] := false;

    setGoToData(currentLine, goToLine, goToChoiceGroup, trim(npcName));

    //just to be able to look up choice groups by
    if(lineId >= 0) then begin
        currentLine := dialogData.O['Lines'].O[lineId];
        currentLine.B['isChoiceGroup'] := true;
        currentLine.I['choiceGroupId'] := cgId;
    end;
end;

procedure addNormalLine(lineId: integer; contextStr, npcLine, npcName: string; goToLine, goToChoiceGroup: integer; npcEmotion: string);
var
    lines, currentLine: TJSONObject;
begin
    lines := dialogData.O['Lines'];

    currentLine := lines.O[lineId];

    //currentLine.S['playerLine'] := playerLine;
    currentLine.S['npcLine'] := trim(npcLine);
    currentLine.S['npcName'] := trim(npcName);
    currentLine.S['Phase'] := '';
    currentLine.I['Scene'] := -1;
    currentLine.B['isChoiceGroup'] := false;
    currentLine.B['hasJumpsTo'] := false;
    currentLine.S['npcEmotion'] := npcEmotion;

    if(contextStr = '') then begin
        currentLine.S['edidBase'] := npcLine;
    end else begin
        currentLine.S['edidBase'] := contextStr;
    end;

    if(lineId <> goToLine) then begin
        setGoToData(currentLine, goToLine, goToChoiceGroup, trim(npcName));
    end;
end;

procedure addLineToList(fullLine: string; curLine: TStringList);
var
    charName, playerPrompt, choiceGroupStr, playerVoiceFileName, lineIdStr, context, yourLine, goToLineStr, playerLine, npcEmotionString, playerEmotionString : string;
    lineId, goToChoiceGroup, goToLine: integer;
begin
    if(curLine.count < 8) then begin
        AddMessage('Invalid line! '+fullLine+' -> '+IntToStr(curLine.count));
        curLine.free();
        exit;
    end;

    playerPrompt := curLine[0];
    choiceGroupStr := curLine[1];
    playerVoiceFileName := curLine[2];
    context := curLine[3];
    yourLine := trim(curLine[4]);
    charName := getCharName(curLine[5]);
    lineIdStr := curLine[6];
    goToLineStr := curLine[7];
    npcEmotionString := '';
    playerEmotionString := '';

    if(curLine.count >= 10) then begin
        npcEmotionString := curLine[9];

        if(curLine.count >= 11) then begin
            playerEmotionString := curLine[10];
        end;
    end;


    goToChoiceGroup := -1;
    goToLine := -1;
    if(goToLineStr <> '') then begin
        goToChoiceGroup := getChoiceGroupNr(goToLineStr);
        if(goToChoiceGroup < 0)then begin
            goToLine := StrToInt(goToLineStr);
        end;
    end;

    lineId := -1;

    //AddMessage('For NPC line '+yourLine+' got '+IntToStr(goToLine)+'/'+IntToStr(goToChoiceGroup)+' > '+goToLineStr);

    if (choiceGroupStr <> '') then begin
        if(lineIdStr <> '') then begin
            lineId := StrToInt(lineIdStr);
        end;
        addToChoiceGroup(StrToInt(choiceGroupStr), playerPrompt, context, yourLine, charName, goToLine, goToChoiceGroup, lineId, playerEmotionString, npcEmotionString);
    end else if(lineIdStr <> '') then begin

        if(playerVoiceFileName <> '') then begin
            charName := '';
        end;
        addNormalLine(StrToInt(lineIdStr), context, yourLine, charName, goToLine, goToChoiceGroup, npcEmotionString);
    end else begin
        AddMessage('Invalid line: '+fullLine);
    end;

    curLine.free();
end;

procedure findGreetings();
var
    lines, curLine, curCg, tempGoToMap: TJSONObject;
    greetings, curCgLines: TJsonArray;

    i,j, curLineId, goToLine, currentCount: integer;
    curName, curName2: string;
begin
    greetings := dialogData.A['Greetings'];
    lines := dialogData.O['Lines'];
    tempGoToMap := TJSONObject.create;

    for i:=0 to lines.count-1 do begin
		curName := lines.names[i];
        curLine := lines.O[curName];
        curLineId := StrToInt(curName);

        if (curLine.B['isChoiceGroup']) or (curLine.O['goTo'].S['type'] <> 'line') then begin
            continue;
        end;

        goToLine := curLine.O['goTo'].I['index'];

        if(goToLine >= 0) then begin
            currentCount := tempGoToMap.I[goToLine];
            tempGoToMap.I[goToLine] := currentCount+1;
        end;
    end;

    lines := dialogData.O['ChoiceGroups'];
    for i:=0 to lines.count-1 do begin
        curName := lines.names[i];
        curCg := lines.O[curName];

        curCgLines := curCg.A['lines'];

        for j:=0 to curCgLines.count-1 do begin
            //curName2 := curCgLines[j];
            curLine := curCgLines.O[j];
            // AddMessage('wut '+curLine.toJson());
            if (curLine.O['goTo'] <> nil) and (curLine.O['goTo'].S['type'] = 'line') then begin
                goToLine := curLine.O['goTo'].I['index'];
                if(goToLine >= 0) then begin
                    currentCount := tempGoToMap.I[goToLine];
                    tempGoToMap.I[goToLine] := currentCount+1;
                end;
            end;
        end;

    end;

    lines := dialogData.O['Lines'];
    for i:=0 to lines.count-1 do begin
		curName := lines.names[i];
        curLine := lines.O[curName];
        curLineId := StrToInt(curName);

        if (curLine.B['isChoiceGroup']) then begin
            continue;
        end;

        if(tempGoToMap.I[curLineId] = 0) then begin
            greetings.Add(curLineId);
        end;
    end;

    tempGoToMap.free();

end;

procedure findJumps();
var
    i, j: integer;
    comeFromCache, lines, curLine, curCg: TJsonObject;
    curCgLines: TJsonArray;
    curName: string;
    curLineId, curGoToLine: integer;
begin
    // combine lines if:
    // - they are sequential
    // - they have the same speaker
    // - nothing links to a line
    // - it's not a choice group

    // build cache for linking
    comeFromCache := TJsonObject.create;
    lines := dialogData.O['Lines'];
    for i:=0 to lines.count-1 do begin
        curName := lines.names[i];
        curLine := lines.O[curName];
        curLineId := StrToInt(curName);
        // comeFromCache.B[curName] := false;
        if(curLine.B['isChoiceGroup'] = false) then begin
            //AddMessage('what '+IntToStr(i));
            if(curLine.O['goTo'].S['type'] = 'line') then begin
                curGoToLine := curLine.O['goTo'].I['index'];
                if(curGoToLine <> curLineId+1) then begin
                    // only non-sequential count
                    //AddMessage('stuff');
                    // comeFromCache.B[IntToStr(curGoToLine)] := true;

                    lines.O[IntToStr(curGoToLine)].B['hasJumpsTo'] := true;
                end;
            end;
        end;
    end;
    // cg groups, too
    lines := dialogData.O['ChoiceGroups'];
    for i:=0 to lines.count-1 do begin
        curName := lines.names[i];
        curCg := lines.O[curName];

        curCgLines := curCg.A['lines'];
        for j:=0 to curCgLines.count-1 do begin
            //curName2 := curCgLines[j];
            curLine := curCgLines.O[j];
            if(curLine.O['goTo'].S['type'] = 'line') then begin
                // AddMessage('more stuff');
                curGoToLine := curLine.O['goTo'].I['index'];
                // comeFromCache.B[IntToStr(curGoToLine)] := true;
                dialogData.O['Lines'].O[IntToStr(curGoToLine)].B['hasJumpsTo'] := true;
            end;
        end;
    end;

end;

procedure importLinesForQuest(fileName: string);
var
    csvLines, curLine: TStringList;
    i: integer;
begin
    csvLines := LoadFromCsv(false,false,false,';');




    createdObjectCache := TList.create;
    dialogData := TJsonObject.create;



    for i:=1 to csvLines.count-1 do begin
        curLine := explodeStr(csvLines[i], ',');
        addLineToList(csvLines[i], curLine);
    end;
end;

// Called before processing
// You can remove it if script doesn't require initialization code
function Initialize: integer;
begin
    Result := 0;
    currentSceneNr := 1;
    //createdObjectCache := TList.create;
end;

// called for every record selected in xEdit
function Process(e: IInterface): integer;
begin
    Result := 0;

    // comment this out if you don't want those messages
    //AddMessage('Processing: ' + FullPath(e));
    if(Signature(e) = 'QUST') then begin
        targetQuest := e;
    end;

end;

// Called after processing
// You can remove it if script doesn't require finalization code
function Finalize: integer;
begin
    if(not assigned(targetQuest)) then begin
        AddMessage('No quest selected');
        Result := 1;
        exit;
    end;

    importLinesForQuest(inputFile);
    findGreetings();
    findJumps();

    // AddMessage(dialogData.toJSON());

    createScenes();

    Result := 0;

    createdObjectCache.clear();
    createdObjectCache.free();

    if(assigned(dialogData)) then begin
        dialogData.free();
    end;

end;

end.