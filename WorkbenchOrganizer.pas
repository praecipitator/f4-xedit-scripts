{
    Creates a patch for Organized Workbench Menu.
    Requires OWM-Master.esl or OWM-Master.esp to be loaded.
}
unit GroupWorkbenches;

    uses praFunctions;
    uses praUtil;

    var
        //smmMainFile: IInterface;
        //smmTemplateFile: IInterface;

        masterFile: IInterface;
        targetFile: IInterface;

        targetFileQuest: IInterface;

        setupDone: boolean;
        targetFileName: String;

        targetFormList: IInterface;

        // if a bench has NO relevant KW
        keywordMiscBench: IInterface;
        keywordChemBench, keywordWeaponBench, keywordArmorBench, keywordCookingStove, keywordPowerArmorBench, keywordRobotBench, keywordNukaMixer: IInterface;

        workbenchKwBlacklist: TStringList;

        // list of keyword which are definitely known to be BNAMs
        knownWorkbenchKws: TStringList;
        // crafting and quest
        fnamsToReplace: TStringList;

        benchKwToSubmenuKeys: TStringList;
        benchKwToSubmenuVals: array [0..256] of IInterface;

        // store the bench's COBJ here!
        unknownBenches: array [0..50] of IInterface;
        unknownBenchesLength: integer;

    ////////////// HACKS BECAUSE THIS PASCAL DOESN'T SUPPORT DYNAMIC ARRAYS //////////////

    // ---- fake hash map ----
    procedure setKwToSubmenu(key: string; submenu: IInterface);
    var
        i: integer;
    begin
        i := benchKwToSubmenuKeys.indexOf(key);
        if(i < 0) then begin
            i := benchKwToSubmenuKeys.add(key);
        end;

        benchKwToSubmenuVals[i] := submenu;
    end;

    function getKwToSubmenu(key: string): IInterface;
    var
        i: integer;
    begin
        Result := nil;

        i := benchKwToSubmenuKeys.indexOf(key);
        if(i >= 0) then begin
            Result := benchKwToSubmenuVals[i];
        end;
    end;

    // ---- unknown benches (COBJ!!!) -----
    procedure addUnknownBench(benchCobj: IInterface);
    begin
        unknownBenches[unknownBenchesLength] := benchCobj;
        unknownBenchesLength := unknownBenchesLength + 1;
    end;

    function findUnknownBench(benchCobj: IInterface): integer;
    var
        curBenchCobj: IInterface;
        i: integer;
    begin
        Result := -1;
        for i:=0 to unknownBenchesLength do begin
            curBenchCobj := unknownBenches[i];
            if(isSameForm(curBenchCobj, benchCobj)) then begin
                Result := i;
                exit;
            end;
        end;
    end;

    function findUnknownBenchByKw(keywordEdid: string): integer;
    var
        curBenchCobj: IInterface;
        curBench: IInterface;
        i: integer;
    begin
        Result := -1;
        for i:=0 to unknownBenchesLength do begin
            curBenchCobj := unknownBenches[i];
            curBench := getCraftResult(curBenchCobj);
            if(hasKeywordByPath(curBench, keywordEdid, 'KWDA')) then begin
                Result := i;
                exit;
            end;
        end;
    end;

    function findUnknownBenchByKwIInterface(keyword: IInterface): integer;
    var
        curBenchCobj: IInterface;
        curBench: IInterface;
        i: integer;
    begin
        Result := -1;
        for i:=0 to unknownBenchesLength do begin
            curBenchCobj := unknownBenches[i];
            curBench := getCraftResult(curBenchCobj);
            if(hasKeywordByPath(curBench, keyword, 'KWDA')) then begin
                Result := i;
                exit;
            end;
        end;
    end;

    procedure removeUnknownBench(benchIndex: integer);
    begin
        // put the last one into the old position
        unknownBenchesLength := unknownBenchesLength - 1;
        unknownBenches[benchIndex] = unknownBenches[unknownBenchesLength];
    end;


    ////////////// ACTUAL CODE //////////////

    procedure addKnownBnamKeyword(kw: string);
    begin

        if (length(kw) > 0) and (knownWorkbenchKws.indexOf(kw) < 0) then begin
            //AddMessage('Adding '+kw+' to the list of known keywords');
            knownWorkbenchKws.add(kw);
        end;
    end;

    function findWorkbenchKeyword(bench: IInteface): IInterface;
    var
        kwda: IInterface;
        curKW: IInterface;
        i: Integer;
        edid: string;

        benchType: string;
        numCandidates: integer;
        potentialResult: IInterface;
    begin
        kwda := ElementBySignature(bench, 'KWDA');
        numCandidates := 0;
        result := nil;

        // weapons, armor and power armor work differently
        benchType := getWorkbenchType(bench);

        // do that first
        if(benchType = 'Weapons') then begin
            Result := keywordWeaponBench;
            exit;
        end;

        if(benchType = 'Armor') then begin
            Result := keywordArmorBench;
            exit;
        end;

        if(benchType = 'Power Armor') then begin
            Result := keywordPowerArmorBench;
            exit;
        end;

        if(benchType = 'Robot Mod') then begin
            Result := keywordRobotBench;
            exit;
        end;


        if(benchType = 'Alchemy') then begin
            for i := 0 to ElementCount(kwda)-1 do begin
                curKW := LinksTo(ElementByIndex(kwda, i));
                edid := GetElementEditValues(curKW, 'EDID');

                // check in the knowns
                if(knownWorkbenchKws.indexOf(edid) >= 0) then begin
                    result := curKW;
                    exit;
                end;

                if not (startsWith('AnimFurn', edid) or startsWith('AnimsFurn', edid) or (workbenchKwBlacklist.indexOf(edid) >= 0)) then begin
                    //AddMessage('we are in the if');
                    // ok
                    numCandidates := numCandidates+1;
                    potentialResult := curKW;
                end;
            end;

            if(numCandidates = 1) then begin
                // all ok
                result := potentialResult;

                addKnownBnamKeyword(edid);
                exit;
            end else if(numCandidates = 0) then begin
                // none
                AddMessage('Found no valid keyword in '+DisplayName(bench));
                result := keywordMiscBench;
                exit;
            end;
            // otherwise return nil
            AddMessage('Found '+inttostr(numCandidates)+' possible keywords in '+DisplayName(bench)+'.');
            //addUnknownBench(
            result := nil;
            exit;
        end;

        // otherwise misc
        result := keywordMiscBench;
    end;

    function getSubmenuByBenchKeyword(benchKw: IInterface; potentialTitle: string): IInterface;
    var
        newSubmenu: IINterface;
        newSubmenuEdid: string;
        benchKwEdid: string;
    begin
        // bench KW might be correct already
        if (
            isSameForm(benchKw, keywordMiscBench) or
            isSameForm(benchKw, keywordChemBench) or
            isSameForm(benchKw, keywordWeaponBench) or
            isSameForm(benchKw, keywordArmorBench) or
            isSameForm(benchKw, keywordCookingStove) or
            isSameForm(benchKw, keywordPowerArmorBench) or
            isSameForm(benchKw, keywordRobotBench) or
            isSameForm(benchKw, keywordNukaMixer)
        ) then begin
            //AddMessage('Bench KW already correct');
            result := benchKw;
            exit;
        end;


        benchKwEdid := GetElementEditValues(benchKw, 'EDID');
        // other stuff
        newSubmenu := getKwToSubmenu(benchKwEdid);

        if(assigned(newSubmenu)) then begin
            AddMessage('Found a menu for '+benchKwEdid);
            result := newSubmenu;
            exit;
        end;



        newSubmenuEdid := 'praWBG_'+benchKwEdid;

        // before creating it, check if this thing exist in our formlist already
        newSubmenu := getFormlistEntryByEdid(targetFormList, newSubmenuEdid);
        if assigned(newSubmenu) then begin
            Result := newSubmenu;
            exit;
        end;

        // check if we have it in targetfile
        newSubmenu := MainRecordByEditorID(GroupBySignature(targetFile, 'KYWD'), newSubmenuEdid);
        if assigned(newSubmenu) then begin
            Result := newSubmenu;
            exit;
        end;

        AddMessage(benchKwEdid+' has no menu, adding a new one');
        // actually copy
        newSubmenu := wbCopyElementToFile(keywordMiscBench, targetFile, True, True);
        setElementEditValues(newSubmenu, 'EDID', newSubmenuEdid);
        setElementEditValues(newSubmenu, 'FULL', potentialTitle);

        setKwToSubmenu(benchKwEdid, newSubmenu);

        Result := newSubmenu;
    end;


    procedure processItemCobj(e: IInterface; craftResult: IInterface);
    var
        bnam: IInterface;
        bnamId: string;
    begin
        // knownWorkbenchKws
        bnam := getBnam(e);
        bnamId := GetElementEditValues(bnam, 'EDID');

        if(startsWith('WorkshopWorkbench', bnamId)) then begin
            // irrelevant
            exit;
        end;


        // otherwise put it into the known list
        addKnownBnamKeyword(bnamId);


    end;

    // check if this is something which we should move to the new menu
    function isWorkbench(cobj: IInterface; craftResult: IInterface): boolean;
    begin
        Result := false;

        // if this is something under the crafting menu, consider it
        if(hasKeywordByPath(cobj, 'WorkshopRecipeFilterCrafting', 'FNAM')) then begin
            Result := true;
            exit;
        end;

        // for the special menu, it has to be a FURN and the workbench keyword
        if(hasKeywordByPath(cobj, 'WorkshopRecipeFilterQuest', 'FNAM') and hasKeywordByPath(craftResult, 'Workbench_General', 'KWDA') and (signature(craftResult) = 'FURN')) then begin
            Result := true;
            exit;
        end;

    end;

    procedure addToQuestArray(newEntry: IInterface);
    var
        curKW, curTemp, curEntry, rootEntry: IInterface;
        curMember: IInterface;
        i, numEntries: integer;
    begin
        // before doing anything else, check if the KW exists in the formlist already
        if (hasFormlistEntry(targetFormList, newEntry)) then begin
            exit;
        end;

        if(not assigned(targetFileQuest)) then begin
            // prepare the quest
            prepareTargetFile(targetFile);
        end;

        // try to iterate through the quest
        rootEntry := ElementByPath(getPropertyOfScript(targetFileQuest, 'pra:OrganizedWorkbenchMenuMain', 'Menus'), 'Value\Array of Struct');

        // I need Value/Array of Object
        numEntries := ElementCount(rootEntry);
        for i:=0 to numEntries-1 do begin
            // should be Struct #i
            curEntry := ElementByIndex(rootEntry, i);
            curTemp := getStructMember(curEntry, 'ModMenu');
            if(assigned(curTemp)) then begin
                curKW := LinksTo(ElementByNamePath(curTemp, 'Object Union\Object v2\FormID'));
            end;
            if(isSameForm(curKw, newEntry)) then begin
                exit;
            end;

            //AddMessage('==');
            // dumpElement(curKW, '');
        end;

        addRequiredMastersSilent(newEntry, targetFile);

        // now append. omg.
        // this creates an empty struct
        curEntry := ElementAssign(rootEntry, HighInteger, nil, False);


        addStructMemberLinkedObject(curEntry, 'ModMenu', newEntry);
        addStructMemberLinkedObject(curEntry, 'TargetMenu', targetFormList);



    end;

    procedure processWorkbenchCobj(e: IInterface; craftResult: IInterface; isFromBacklog: boolean);
    var
        workbenchKeyword: IInterface;
        submenuKw: IInterface;
        cobjOverride: IInterface;
    begin

        // failsafe, check if we even have a bnam
        if not (assigned(ElementBySignature(e, 'BNAM'))) then begin
            AddMessage(DisplayName(craftResult)+' has no BNAM');
            exit;
        end;


        //getWorkbenchType(craftResult);
        // try to fish out the relevant KW
        workbenchKeyword := findWorkbenchKeyword(craftResult);
        if(not assigned(workbenchKeyword)) then begin
            // put it into the backlog
            if(isFromBacklog) then begin
                //AddMessage('Still ');
                // put it into misc
                workbenchKeyword := keywordMiscBench;
            end else begin
                AddMessage('Adding '+DisplayName(craftResult)+' to backlog');
                addUnknownBench(e);
                exit;
            end;
        end;

        //AddMessage('Getting submenu KW for '+DisplayName(craftResult));
        submenuKw := getSubmenuByBenchKeyword(workbenchKeyword, DisplayName(craftResult));
        //dumpElement(submenuKw, '');
        //AddMessage('Got '+DisplayName(submenuKw));

        addToQuestArray(submenuKw);

        AddMessage('Moving '+DisplayName(craftResult)+' to submenu "'+DisplayName(submenuKw)+'"');
        // now edit the cobj
        { addRequiredMastersSilent(e, TargetFile, False); }
        { cobjOverride := wbCopyElementToFile(e, TargetFile, False, True); }

        cobjOverride := getOrCreateElementOverride(e, TargetFile);

        //AddMessage():
        // change the FNAM, not the BNAM
        // setBnam(cobjOverride, submenuKw);
        addRequiredMastersSilent(submenuKw, TargetFile);
        replaceAnyFnam(cobjOverride, fnamsToReplace, submenuKw, false);

    end;

    function findOWMQuest(inFile: IInterface): IInterface;
    var
        numRecords, i: integer;
        curRecord: IInterface;
        questEdid: string;
    begin
        numRecords := RecordCount(inFile);
        Result := nil;

        for i := 0 to numRecords-1 do begin
            curRecord := RecordByIndex(inFile, i);
            // is this a quest?
            if(signature(curRecord) = 'QUST') then begin
                // does it have my script?
                if(hasScriptMulti(curRecord, 'pra:OrganizedWorkbenchMenuMain', false)) then begin
                    // is it mine?
                    if(startsWith('pra_OWMPlugin_', GetElementEditValues(curRecord, 'EDID'))) then begin
                        // found
                        Result := curRecord;
                        exit;
                    end;
                end;
            end;
        end;
    end;

    function getQuestEdidForFile(inFile: IInterface): string;
    begin
        Result := 'pra_OWMPlugin_'+StringReplace(GetFileName(inFile), '.', '-', [rfReplaceAll]);
    end;

    // this should copy the necessary stuff into the target file, like the quest
    procedure prepareTargetFile(inFile: IInterface);
    var
        quest: IInterface;
        baseQuest: IInterface;
        rootEntry: IInterface;
        inFileName: string;
    begin
        // first, ensure masters
        addRequiredMastersSilent(masterFile, inFile);

        // first, check if the script has the quest already
        quest := findOWMQuest(targetFile);
        inFileName := GetFileName(inFile);

        if(not assigned(quest)) then begin
            // seems to be missing. create it.
            baseQuest := MainRecordByEditorID(GroupBySignature(masterFile, 'QUST'), 'pra_SmmOrganizedMenuInstaller');
            quest := wbCopyElementToFile(baseQuest, inFile, true, true);
            // change it's EDID right away
            SetElementEditValues(quest, 'EDID', getQuestEdidForFile(inFile));

            // change it's settings
            rootEntry := ElementByPath(getPropertyOfScript(quest, 'pra:OrganizedWorkbenchMenuMain', 'Menus'), 'Value\Array of Struct');
            while(ElementCount(rootEntry) > 0) do begin
                RemoveElement(rootEntry, 0);
            end;

            // now fix the other stuff
            setPropertyOfScriptValue(quest, 'pra:OrganizedWorkbenchMenuMain', 'ModName', 'Automatic OWM patch '+inFileName);
            setPropertyOfScriptValue(quest, 'pra:OrganizedWorkbenchMenuMain', 'Author', 'WorkbenchOrganizer.pas');
        end;

        // always update this, in case the user has renamed the file (converted it to ESL, for example)
        setPropertyOfScriptValue(quest, 'pra:OrganizedWorkbenchMenuMain', 'PluginName', inFileName);
        // always update the version, too
        setPropertyOfScriptValue(quest, 'pra:OrganizedWorkbenchMenuMain', 'currentVersion', IntToStr(getUnixTimestamp()));

        targetFileQuest := quest;


    end;

    function doSetup(): boolean;
    var


        kwMainGroup: IInterface;
    begin
        Result := false;

        masterFile := findFileEspOrEsl('OWM-Master', true);
        if(not assigned(masterFile)) then begin
            exit;
        end;

        targetFile := showFileSelectionDialog(masterFile);
        if(not assigned(targetFile)) then begin
            exit;
        end;
        //prepareTargetFile(targetFile);
        targetFileName := GetFileName(targetFile);

        kwMainGroup := GroupBySignature(masterFile, 'KYWD');

        keywordChemBench      := MainRecordByEditorID(kwMainGroup, 'praWBG_Chemistry');
        keywordWeaponBench    := MainRecordByEditorID(kwMainGroup, 'praWBG_Weapons');
        keywordArmorBench     := MainRecordByEditorID(kwMainGroup, 'praWBG_Armor');
        keywordCookingStove   := MainRecordByEditorID(kwMainGroup, 'praWBG_Cooking');
        keywordPowerArmorBench:= MainRecordByEditorID(kwMainGroup, 'praWBG_PowerArmor');
        keywordRobotBench     := MainRecordByEditorID(kwMainGroup, 'praWBG_Robot');
        keywordNukaMixer      := MainRecordByEditorID(kwMainGroup, 'praWBG_Nukacola');
        keywordMiscBench      := MainRecordByEditorID(kwMainGroup, 'praWBG_Misc');

        targetFormList := MainRecordByEditorID(GroupBySignature(masterFile, 'FLST'), 'praWorkshopMenuCraftingGrouped');

        //knownWorkbenchKws.add('WorkbenchCooking');

        // register them for the vanilla
        setKwToSubmenu('DLC04_WorkbenchSoda', keywordNukaMixer);
        setKwToSubmenu('WorkbenchChemlab', keywordChemBench);
        setKwToSubmenu('WorkbenchCooking', keywordCookingStove);

        Result := true;
        setupDone := true;
    end;

    procedure loadKnownBnamFile();
    var
        dir: String;
        Path: String;
        pattern: String;
        Attr: Integer;
        FileAttr: Integer;
        Res: TSearchRec;
        name: String;
    begin
        name := ProgramPath + 'Edit Scripts\known-bnam-keywords.txt';
        FileAttr := FileGetAttr(name);
        // what is faDirecotry even?
        if FileAttr and faDirectory = 0 then
         begin
            { Do something with file name }
            AddMessage('Loading stuff from '+Name);
            knownWorkbenchKws.loadFromFile(Name);
            //list := TStringList.Create;
            //list.loadFromFile(filename);
            // loadOverrides(Name);
         end;


    end;

    procedure saveKnownBnamFile();
    begin
        knownWorkbenchKws.SaveToFile(ProgramPath + 'Edit Scripts\known-bnam-keywords.txt');
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        setupDone := false;

        benchKwToSubmenuKeys := TSTringList.create;
        workbenchKwBlacklist := TSTringList.create;
        knownWorkbenchKws := TSTringList.create;
        fnamsToReplace := TSTringList.create;

        workbenchKwBlacklist.add('Workbench_General');
        workbenchKwBlacklist.add('FurnitureForce3rdPerson');
        workbenchKwBlacklist.add('FurnitureScaleActorToOne');
        workbenchKwBlacklist.add('WorkshopWorkObject');
        workbenchKwBlacklist.add('FurnitureClassWork');
        workbenchKwBlacklist.add('kgSIM_PreventAutoAssign');

        fnamsToReplace.add('WorkshopRecipeFilterCrafting');
        fnamsToReplace.add('WorkshopRecipeFilterQuest');



        //setLength(foobarTest, 5);

        // load known keywords
        loadKnownBnamFile();

        addKnownBnamKeyword('WorkbenchChemlab');
        addKnownBnamKeyword('WorkbenchCooking');
        addKnownBnamKeyword('DLC04_WorkbenchSoda');

    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        curCraftResult: IInterface;
        curCraftResultSig: string;
        curFileName: string;
    begin
        Result := 0;

        if(not setupDone) then begin
            if(not doSetup()) then begin
                Result := 1;
                exit;
            end;
        end;

        // only do COBJ
        if(signature(e) <> 'COBJ') then begin
            exit;
        end;

        // do not process anything from myself
        curFileName := GetFileName(GetFile(e));

        if (curFileName = GetFileName(masterFile)) or (curFileName = targetFileName) then begin
            exit; // don't do this
        end;

        curCraftResult := getCraftResult(e);
        if(not assigned(curCraftResult)) then begin
            exit;
        end;

        if(isWorkbench(e, curCraftResult)) then begin
            processWorkbenchCobj(e, curCraftResult, false);
        end else begin
            processItemCobj(e, curCraftResult);
        end;

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        i : integer;
        craftResult: IInterface;
        cur: IInterface;
    begin
        Result := 0;

        AddMessage(IntToStr(unknownBenchesLength)+' benches in the backlog');

        for i:=0 to unknownBenchesLength-1 do begin
            cur := unknownBenches[i];
            craftResult := getCraftResult(cur);
            processWorkbenchCobj(cur, craftResult, true);
        end;

        saveKnownBnamFile();
    end;

end.
