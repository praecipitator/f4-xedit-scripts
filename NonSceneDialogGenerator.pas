{
    Run on a quest, then select CSV to import the lines.
    Can't remember how it worked or why I even made this...
}
unit NonSceneDialogGenerator;

    uses praUtil;

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

    function getSubtypeName(topic: string): string;
    begin
        Result := '';
        if(topic = 'ZKeyObject') then begin
            Result := 'ZKEY';
            exit;
        end;
        if(topic = 'WaitingForPlayerInput') then begin
            Result := 'WFPI';
            exit;
        end;
        if(topic = 'TimeToGo') then begin
            Result := 'TITG';
            exit;
        end;
        if(topic = 'SwingMeleeWeapon') then begin
            Result := 'SWMW';
            exit;
        end;
        if(topic = 'StandonFurniture') then begin
            Result := 'STOF';
            exit;
        end;
        if(topic = 'ShootBow') then begin
            Result := 'FIWE';
            exit;
        end;
        if(topic = 'PursueIdleTopic') then begin
            Result := 'PURS';
            exit;
        end;
        if(topic = 'PlayerShout') then begin
            Result := 'PCSH';
            exit;
        end;
        if(topic = 'PlayerinIronSights') then begin
            Result := 'PIRN';
            exit;
        end;
        if(topic = 'PlayerCastSelfSpell') then begin
            Result := 'PCSS';
            exit;
        end;
        if(topic = 'PlayerCastProjectileSpell') then begin
            Result := 'PCPS';
            exit;
        end;
        if(topic = 'PlayerAquireFeaturedItem') then begin
            Result := 'PAFI';
            exit;
        end;
        if(topic = 'PlayerActivateTerminals') then begin
            Result := 'PATR';
            exit;
        end;
        if(topic = 'PlayerActivateFurniture') then begin
            Result := 'PAFU';
            exit;
        end;
        if(topic = 'PlayerActivateDoor') then begin
            Result := 'PADR';
            exit;
        end;
        if(topic = 'PlayerActivateContainer') then begin
            Result := 'PACO';
            exit;
        end;
        if(topic = 'PlayerActivateActivators') then begin
            Result := 'PAAC';
            exit;
        end;
        if(topic = 'PickpocketTopic') then begin
            Result := 'PICT';
            exit;
        end;
        if(topic = 'OutofBreath') then begin
            Result := 'OUTB';
            exit;
        end;
        if(topic = 'ObserveCombat') then begin
            Result := 'OBCO';
            exit;
        end;
        if(topic = 'NoticeCorpse') then begin
            Result := 'NOTI';
            exit;
        end;
        if(topic = 'LockedObject') then begin
            Result := 'LOOB';
            exit;
        end;
        if(topic = 'LeaveWaterBreath') then begin
            Result := 'LWBS';
            exit;
        end;
        if(topic = 'KnockOverObject') then begin
            Result := 'KNOO';
            exit;
        end;
        if(topic = 'Jump') then begin
            Result := 'JUMP';
            exit;
        end;
        if(topic = 'ImpatientQuestion') then begin
            Result := 'IMQU';
            exit;
        end;
        if(topic = 'ImpatientPostitive') then begin
            Result := 'IMPT';
            exit;
        end;
        if(topic = 'ImpatientNeutral') then begin
            Result := 'IMNU';
            exit;
        end;
        if(topic = 'ImpatientNegative') then begin
            Result := 'IMNG';
            exit;
        end;
        if(topic = 'Idle') then begin
            Result := 'IDLE';
            exit;
        end;
        if(topic = 'Goodbye') then begin
            Result := 'GBYE';
            exit;
        end;
        if(topic = 'ExitBowZoomBreath') then begin
            Result := 'EXBZ';
            exit;
        end;
        if(topic = 'EnterSprintBreath') then begin
            Result := 'BREA';
            exit;
        end;
        if(topic = 'EnterBowZoomBreath') then begin
            Result := 'ENBZ';
            exit;
        end;
        if(topic = 'DestroyObject') then begin
            Result := 'DEOB';
            exit;
        end;
        if(topic = 'CombatGrunt') then begin
            Result := 'GRNT';
            exit;
        end;
        if(topic = 'ActorCollidewithActor') then begin
            Result := 'ACAC';
            exit;
        end;
        if(topic = 'SharedInfo') then begin
            Result := 'IDAT';
            exit;
        end;
        if(topic = 'Hello') then begin
            Result := 'HELO';
            exit;
        end;
        if(topic = 'Greeting') then begin
            Result := 'GREE';
            exit;
        end;
        if(topic = 'Travel') then begin
            Result := 'TRAV';
            exit;
        end;
        if(topic = 'TrainingExit') then begin
            Result := 'TREX';
            exit;
        end;
        if(topic = 'Training') then begin
            Result := 'TRAI';
            exit;
        end;
        if(topic = 'ServiceRefusal') then begin
            Result := 'SERU';
            exit;
        end;
        if(topic = 'RepairExit') then begin
            Result := 'REEX';
            exit;
        end;
        if(topic = 'Repair') then begin
            Result := 'REPA';
            exit;
        end;
        if(topic = 'RechargeExit') then begin
            Result := 'RCEX';
            exit;
        end;
        if(topic = 'Recharge') then begin
            Result := 'RECH';
            exit;
        end;
        if(topic = 'BarterExit') then begin
            Result := 'BAEX';
            exit;
        end;
        if(topic = 'NormalToLost') then begin
            Result := 'NOTL';
            exit;
        end;
        if(topic = 'NormalToCombat') then begin
            Result := 'NOTC';
            exit;
        end;
        if(topic = 'NormalToAlert') then begin
            Result := 'NOTA';
            exit;
        end;
        if(topic = 'LostToNormal') then begin
            Result := 'LOTN';
            exit;
        end;
        if(topic = 'LostToCombat') then begin
            Result := 'LOTC';
            exit;
        end;
        if(topic = 'LostIdle') then begin
            Result := 'LOIL';
            exit;
        end;
        if(topic = 'DetectFriendDie') then begin
            Result := 'DFDA';
            exit;
        end;
        if(topic = 'CombatToNormal') then begin
            Result := 'COTN';
            exit;
        end;
        if(topic = 'CombatToLost') then begin
            Result := 'COLO';
            exit;
        end;
        if(topic = 'AlertToNormal') then begin
            Result := 'ALTN';
            exit;
        end;
        if(topic = 'AlertToCombat') then begin
            Result := 'ALTC';
            exit;
        end;
        if(topic = 'AlertIdle') then begin
            Result := 'ALIL';
            exit;
        end;
        if(topic = 'Trade') then begin
            Result := 'TRAD';
            exit;
        end;
        if(topic = 'Show') then begin
            Result := 'SHOW';
            exit;
        end;
        if(topic = 'Refuse') then begin
            Result := 'REFU';
            exit;
        end;
        if(topic = 'PathingRefusal') then begin
            Result := 'PRJT';
            exit;
        end;
        if(topic = 'MoralRefusal') then begin
            Result := 'MREF';
            exit;
        end;
        if(topic = 'ExitFavorState') then begin
            Result := 'FEXT';
            exit;
        end;
        if(topic = 'Agree') then begin
            Result := 'AGRE';
            exit;
        end;
        if(topic = 'Yield') then begin
            Result := 'YIEL';
            exit;
        end;
        if(topic = 'VoicePowerStartShort') then begin
            Result := 'VPSS';
            exit;
        end;
        if(topic = 'VoicePowerStartLong') then begin
            Result := 'VPSL';
            exit;
        end;
        if(topic = 'VoicePowerEndShort') then begin
            Result := 'VPES';
            exit;
        end;
        if(topic = 'VoicePowerEndLong') then begin
            Result := 'VPEL';
            exit;
        end;
        if(topic = 'UNUSED01') then begin
            Result := 'WTCR';
            exit;
        end;
        if(topic = 'TrespassAgainstNC') then begin
            Result := 'TRAN';
            exit;
        end;
        if(topic = 'Trespass') then begin
            Result := 'TRES';
            exit;
        end;
        if(topic = 'ThrowGrenade') then begin
            Result := 'THGR';
            exit;
        end;
        if(topic = 'Taunt') then begin
            Result := 'TAUT';
            exit;
        end;
        if(topic = 'SuppressiveFire') then begin
            Result := 'BGST';
            exit;
        end;
        if(topic = 'StealFromNC') then begin
            Result := 'STFN';
            exit;
        end;
        if(topic = 'Steal') then begin
            Result := 'STEA';
            exit;
        end;
        if(topic = 'Retreat') then begin
            Result := 'FLBK';
            exit;
        end;
        if(topic = 'PowerAttack') then begin
            Result := 'POAT';
            exit;
        end;
        if(topic = 'PickpocketNC') then begin
            Result := 'PICN';
            exit;
        end;
        if(topic = 'PickpocketCombat') then begin
            Result := 'PICC';
            exit;
        end;
        if(topic = 'PairedAttack') then begin
            Result := 'PATT';
            exit;
        end;
        if(topic = 'OrderTakeCover') then begin
            Result := 'ORTC';
            exit;
        end;
        if(topic = 'OrderMoveUp') then begin
            Result := 'ORAV';
            exit;
        end;
        if(topic = 'OrderFlank') then begin
            Result := 'ORFL';
            exit;
        end;
        if(topic = 'OrderFallback') then begin
            Result := 'ORFB';
            exit;
        end;
        if(topic = 'MurderNC') then begin
            Result := 'MUNC';
            exit;
        end;
        if(topic = 'Murder') then begin
            Result := 'MURD';
            exit;
        end;
        if(topic = 'Hit') then begin
            Result := 'HIT_';
            exit;
        end;
        if(topic = 'Flee') then begin
            Result := 'FLEE';
            exit;
        end;
        if(topic = 'Death') then begin
            Result := 'DETH';
            exit;
        end;
        if(topic = 'CrippledLimb') then begin
            Result := 'CRIL';
            exit;
        end;
        if(topic = 'CoverMe') then begin
            Result := 'RQST';
            exit;
        end;
        if(topic = 'Block') then begin
            Result := 'BLOC';
            exit;
        end;
        if(topic = 'BleedOut') then begin
            Result := 'BLED';
            exit;
        end;
        if(topic = 'Bash') then begin
            Result := 'BASH';
            exit;
        end;
        if(topic = 'AvoidThreat') then begin
            Result := 'AVTH';
            exit;
        end;
        if(topic = 'Attack') then begin
            Result := 'ATCK';
            exit;
        end;
        if(topic = 'AssaultNC') then begin
            Result := 'ASNC';
            exit;
        end;
        if(topic = 'Assault') then begin
            Result := 'ASSA';
            exit;
        end;
        if(topic = 'AllyKilled') then begin
            Result := 'ALKL';
            exit;
        end;
        if(topic = 'AcceptYield') then begin
            Result := 'ACYI';
            exit;
        end;

    end;

    function createDial(topic: string; quest: IInterface): IInterface;
    var
        dialData, curDial, questGroup: IInterface;
        subtypeName : string;
        i: integer;
    begin
        Result := nil;
        subtypeName := getSubtypeName(topic);
        if(subtypeName = '') then begin
            AddMessage('Could not find subtype name for '+topic);
            exit;
        end;
        
        questGroup := ChildGroup(quest);
        // see if we have this DIAL already
        for i:=0 to ElementCount(questGroup)-1 do begin
            curDial := ElementByIndex(questGroup, i);
            if(Signature(curDial) = 'DIAL') then begin
                if(GetElementEditValues(curDial, 'SNAM') = subtypeName) then begin
                    AddMessage('found existing dial');
                    Result := curDial;
                    exit;
                end;
            end;
        end;
        

        Result := Add(quest, 'DIAL', true);

        dialData := EnsurePath(Result, 'DATA');
        SetLinksTo(EnsurePath(Result, 'QNAM'), quest);

        SetElementEditValues(dialData, 'Category', 'Player');
        SetElementEditValues(dialData, 'Subtype', topic);

        SetElementEditValues(Result, 'SNAM', subtypeName);
    end;

    function UpperCaseFirstChar(str: string): string;
    var
        firstChar, rest: string;
        len: integer;
    begin
        len := length(str);

        firstChar := copy(str, 0, 1);
        rest := copy(str, 2, len);

        Result := UpperCase(firstChar) + LowerCase(rest);
    end;


    function getEmotionKeyword(emotion: string): IInterface;
    var
        edid: string;
    begin
        Result := nil;

        if(emotion = '') then exit;

        edid := 'AnimFaceArchetype'+UpperCaseFirstChar(emotion);

        // AddMessage('Emotion KW is '+edid);
        Result := FindObjectByEdid(edid);

    end;
    
    function getExistingGroup(dial: IInterface; groupName: string): IInterface;
    var
        curInfo, subGroup: IInterface;
        i: integer;
        flags: cardinal;
    begin
        subGroup := ChildGroup(dial);
        for i:=0 to ElementCount(subGroup)-1 do begin
            curInfo := ElementByIndex(subGroup, i);
            // is group?
            flags := StrToInt(GetElementEditValues(curInfo, 'Record Header\Record Flags'));
            if ((flags and 1) <> 0) then begin
                if(GetElementEditValues(curInfo, 'EDID') = groupName) then begin
                    AddMessage('Found group '+groupName);
                    Result := curInfo;
                    exit;
                end;
            end;
        end;
    end;

    function generateInfos(topic, groupName: string; quest, previous, dial: IInterface; groupArray: TJsonArray): IInterface;
    var
        groupInfo, prevInfo, enamFlags, recordFlags, responses, curRsp, trda: IInterface;
        curData: TJsonObject;
        i: integer;
        faceKw: IInterface;
    begin
        //dial := createDial(topic, quest);

        AddMessage('GenerateInfos: '+topic+', groupName='+groupName);

        if(groupName <> '') then begin
            groupInfo := getExistingGroup(dial, groupName);
            if(not assigned(groupInfo)) then begin
                // create the group
                groupInfo := Add(dial, 'INFO', true);
                SetElementEditValues(groupInfo, 'EDID', groupName);
                enamFlags := EnsurePath(groupInfo, 'ENAM\Flags');
                SetElementEditValues(enamFlags, 'Random', 1);
                SetElementEditValues(enamFlags, 'Player Address', 1);
                SetElementEditValues(enamFlags, 'Unknown 9', 1);
                SetElementEditValues(groupInfo, 'ENAM\Reset Hours', '0.000000');
                
                SetElementEditValues(groupInfo, 'Record Header\Record Flags', '0000001');
                SetElementEditValues(groupInfo, 'INAM', 'Low');

                SetPathLinksTo(groupInfo, 'PNAM', previous);
                previous := groupInfo;
            end;
        end;

        for i:=0 to groupArray.count-1 do begin
            curData := groupArray.O[i];

            AddMessage('Trying to make line for: '+curData.toString());

            Result := Add(dial, 'INFO', true);
            EnsurePath(Result, 'ENAM\Flags');
            SetElementEditValues(Result, 'ENAM\Reset Hours', '0.000000');
            
            SetElementEditValues(Result, 'INAM', 'Low');

            if(assigned(groupInfo)) then begin
                SetPathLinksTo(Result, 'GNAM', groupInfo);
            end;

            SetPathLinksTo(Result, 'PNAM', previous);

            responses := EnsurePath(Result, 'Responses');

            curRsp := ElementByIndex(responses, 0);
            SetElementEditValues(curRsp, 'NAM1', curData.S['line']);

            if(curData.S['faceKw'] <> '') then begin
                faceKw := getEmotionKeyword(curData.S['faceKw']);
                trda :=  EnsurePath(curRsp, 'TRDA');
                if(assigned(faceKw)) then begin
                    SetPathLinksTo(trda, 'Emotion', faceKw);
                end else begin
                    SetElementEditValues(trda, 'Emotion', 'FFFF - None Reference [FFFFFFFF]');
                end;

                SetElementEditValues(trda, 'Response Number', '1');
                SetElementEditValues(trda, 'Unknown', '00 00');
                SetElementEditValues(trda, 'Interrupt Percentage', '0');
                SetElementEditValues(trda, 'Camera Target Alias', '-1');
                SetElementEditValues(trda, 'Camera Location Alias', '-1');
            end;

            previous := Result;
            // prevInfo
        end;
    end;

    procedure generateLines(quest: IInterface; lineData: TJsonObject);
    var
        i: integer;
        topic, groupName: string;
        topicData, groups: TJsonObject;
        curLineArray, curGroupArray: TJsonArray;
        dial, prev: IInterface;
    begin

        for i:=0 to lineData.count-1 do begin
            topic := lineData.names[i];
            topicData := lineData.O[topic];

            dial := createDial(topic, quest);

            // do groupless first
            groupName := '';
            AddMessage('Generate groupless');
            curLineArray := topicData.A['groupless'];
            prev := generateInfos(topic, '', quest, nil, dial, curLineArray);

            groups := topicData.O['groups'];
            for i:=0 to groups.count-1 do begin
                groupName := groups.names[i];
                AddMessage('Generate group '+groupName);
                curGroupArray := groups.A[groupName];

                prev := generateInfos(topic, groupName, quest, prev, dial, curGroupArray);
            end;
        end;
    end;

    procedure processQuest(quest: IInterface);
    var
        csvLines, curLine: TStringList;
        i: integer;
        groupCache: TList;
        topic, groupName, line, faceKw: string;
        lineData, groupArray, groupData: TJsonObject;
    begin
        //createdObjectCache := TList.create;

        lineData := TJsonObject.create;

        csvLines := LoadFromCsv(false,false,false,';');

        for i:=1 to csvLines.count-1 do begin
            curLine := explodeStr(csvLines[i], ',');
            if(curLine.count >= 4) then begin
                topic     := trim(curLine[0]);
                groupName := trim(curLine[1]);
                line      := trim(curLine[2]);
                faceKw    := trim(curLine[3]);

                if(groupName <> '') then begin
                    groupArray := lineData.O[topic].O['groups'].A[groupName];
                end else begin
                    groupArray := lineData.O[topic].A['groupless'];
                end;

                groupData := groupArray.addObject();
                groupData['line'] := line;
                groupData['faceKw'] := faceKw;
            end;
            curLine.free();
        end;

       // createdObjectCache.free();
        csvLines.free();

        AddMessage('JSON: '+lineData.toString());
        generateLines(quest, lineData);
        lineData.free();
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    begin
        Result := 0;

        // comment this out if you don't want those messages
        if(Signature(e) = 'QUST') then begin
            AddMessage('Processing: ' + FullPath(e));
            processQuest(e);
        end;

        // processing code goes here

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
    end;

end.