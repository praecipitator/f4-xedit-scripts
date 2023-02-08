{
    Generates multiple sorting patches for the items it is run on.
    Which patches exactly to generate can be selected in the dialog.
    The patches will be named BaseName-SortingMod.esp, for example MyMod-FIS.esp, MyMod-VIS.esp, etc
}
unit MakeSortingPatches;

    //uses VisOverrides;
    uses praFunctions;
    uses praTagifier;

    const
        taggingConfigDataFileName = ProgramPath + 'Edit Scripts\SortingPatchData.json';
        typeConfigFileName = ProgramPath + 'Edit Scripts\sorting-patches-type-map.txt';
        configFileName = ProgramPath + 'Edit Scripts\multipatcher.json';
        tagExtractionRegexNoBrackets = '^[\[\]\(\)\{\}|]([^\[\]\(\)\{\}|]+)[\[\]\(\)\{\}|].*';
        tagStripRegex = '^[\[\]\(\)\{\}|].+[\[\]\(\)\{\}|] (.+)$';

    var
        ToFile: IInterface;
        taggingConfigData: TJsonObject;
        fileMap, typeConfigKeys, typeConfigValues: TStringList;
        patcherConfig: TJsonObject;

    procedure loadConfig();
    begin
        patcherConfig := TJsonObject.create;
        if(FileExists(configFileName)) then begin
            patcherConfig.loadFromFile(configFileName);
        end;
    end;

    procedure saveConfig();
    begin
        patcherConfig.saveToFile(configFileName, false);
    end;

    function showConfigDialog(): boolean;
    var
        i, createdIndex: integer;
        frm: TForm;
        btnOk, btnCancel: TButton;
        resultCode: integer;
        typeSelect: TComboBox;
        entries: TStringList;
        baseNameInput: TEdit;
        name, baseName: string;
        clbAssets : TCheckListBox;
        hasAtLeastOne: boolean;
        keepTagsCb, dryRunCb: TCheckBox;
    begin
        Result := false;
        frm := CreateDialog('Multipatch', 400, 300);

        CreateLabel(frm, 8, 10, 'Generate for these mods:');
        clbAssets := TCheckListBox.Create(frm);
        clbAssets.Parent := frm;
        clbAssets.Top := 30;
        clbAssets.Left := 8;
        clbAssets.Width := 200;
        clbAssets.Height := 170;
        //clbAssets.Items.AddStrings(slAssetsType);
        //clbAssets.CheckAll(cbChecked, False, False);
        for i:=0 to taggingConfigData.count-1 do begin
            name := taggingConfigData.names[i];
            createdIndex := clbAssets.Items.add(name);
            if(patcherConfig.O['enabledTaggers'].B[name]) then begin
                clbAssets.Checked[createdIndex] := True;
            end;
        end;

        CreateLabel(frm, 230, 10, 'Patch filename base:');
        baseNameInput := CreateInput(frm, 230, 30, patcherConfig.S['baseName']);
        baseNameInput.Width := 150;

        keepTagsCb := CreateCheckbox(frm, 230, 70, 'Keep Valid Tags');
        keepTagsCb.checked := patcherConfig.B['keepValidTags'];

        dryRunCb := CreateCheckbox(frm, 230, 90, 'Dry-Run Mode');
        dryRunCb.checked := patcherConfig.B['dryRunMode'];

        btnOk     := CreateButton(frm, 100, 240, ' OK ');
        btnCancel := CreateButton(frm, 200, 240, 'Cancel');

        btnCancel.ModalResult := mrCancel;

        btnOk.ModalResult := mrYes;
        btnOk.Default := true;

        resultCode := frm.showModal();
        if(resultCode = mrYes) then begin

            patcherConfig.B['keepValidTags'] := keepTagsCb.checked;
            patcherConfig.B['dryRunMode'] := dryRunCb.checked;

            hasAtLeastOne := false;
            for i:=0 to clbAssets.Items.count-1 do begin
                name := clbAssets.Items[i];
                if(clbAssets.Checked[i]) then begin
                    hasAtLeastOne := true;
                end;
                patcherConfig.O['enabledTaggers'].B[name] := clbAssets.Checked[i];
            end;

            baseName := trim(baseNameInput.text);

            patcherConfig.S['baseName'] := baseName;
            if(hasAtLeastOne) and (baseName <> '') then begin
                Result := true;
            end;

        end;
        frm.free();
    end;

    function extractBareTag(text: string): string;
    begin
        Result := regexExtract(text, tagExtractionRegexNoBrackets, 1);
    end;

    procedure loadTaggingConfigData();
    var
        i, j: integer;
        name, tagName, tagValue: string;
        curEntry, entryTags: TJsonObject;
        curTagArray: TJsonArray;
    begin
        taggingConfigData := TJsonObject.create();
        taggingConfigData.LoadFromFile(taggingConfigDataFileName);

        for i:=0 to taggingConfigData.count-1 do begin
            name := taggingConfigData.names[i];
            curEntry := taggingConfigData.O[name];
            curTagArray := curEntry.A['extraValidTags'];
            entryTags := curEntry.O['tags'];
            for j:= 0 to entryTags.count-1 do begin
                tagName := entryTags.Names[j];
                tagValue := extractBareTag(entryTags.S[tagName]);
                curTagArray.add(tagValue);
            end;
        end;
    end;

    function getTypeFromConfig(edid: string): String;
    var
        index: integer;
    begin
        Result := '';
        index := typeConfigKeys.indexOf(edid);
        if(index < 0) then exit;

        Result := typeConfigValues[index];
    end;

    procedure loadTypeConfig();
    var
        tmpList: TStringList;
        i, j, curIndex: integer;
        curLine, curKey, curVal: string;
        breakPos: integer;
    begin
        typeConfigKeys := TStringList.create;
        typeConfigValues := TStringList.create;
        if(not FileExists(typeConfigFileName)) then begin
            exit;
        end;
        tmpList := TStringList.create;
        tmpList.loadFromFile(typeConfigFileName);

        for i:=0 to tmpList.count-1 do begin
            curLine := tmpList[i];
            breakPos := -1;

            for j:=1 to length(curLine) do begin
                if(curLine[j] = '=') then begin
                    breakPos := j;
                    break;
                end;
            end;

            if breakPos <> -1 then begin
                curKey := trim(copy(curLine, 0, breakPos-1));
                curVal := trim(copy(curLine, breakPos+1, length(curLine)));

                curIndex := typeConfigKeys.indexOf(curKey);
                if(curIndex >= 0) then begin
                    typeConfigValues[curIndex] := curVal;
                end else begin
                    curIndex := typeConfigKeys.add(curKey);
                    typeConfigValues.insert(curIndex, curVal);
                end;
            end;
        end;


        tmpList.free();
    end;

    procedure prepareFiles(baseName: string);
    var
        i: integer;
        taggingName, curFileName: string;
        curFile: IInterface;
    begin
        fileMap := TStringList.create;
        for i:=0 to taggingConfigData.count-1 do begin
            taggingName := taggingConfigData.names[i];
            if(patcherConfig.O['enabledTaggers'].B[taggingName]) then begin
                curFileName := baseName + '-' + taggingName + '.esp';
                curFile := FindFile(curFileName);
                if(not assigned(curFile)) then begin
                    curFile := AddNewFileName(curFileName, true);
                end;
                fileMap.addObject(taggingName, curFile);
            end;
        end;
    end;

    function Initialize: integer;
    var
        outFileBase: string;
    begin
        Result := 1;
        initTagifier();
        //initTags();
        loadTaggingConfigData();
        loadTypeConfig();
        loadConfig();
        if(not showConfigDialog()) then begin
            exit;
        end;
        saveConfig();

        prepareFiles(patcherConfig.S['baseName']);
        Result := 0;
    end;

    function getArmoType(e: IInterface): string;
    var
        armorRating: integer;
        armorRatingStr: String;
        armoInrd: IInterface;
        bod2tags: String;
        bod2flags: cardinal;
    begin
        Result := '';
        armoInrd := LinksTo(ElementBySignature(e, 'INRD'));

        if(checkInnr(armoInrd)) then begin
            // nothing to do
            exit;
        end;


        bod2flags := 0;

        // check pipboy
        bod2tags := GetElementEditValues(e, 'BOD2\First Person Flags');
        if(bod2tags <> '') then begin
            // it seems that these flags are in the wrong order...
            bod2tags := reverseString(bod2tags);
            bod2flags := BinToInt(bod2tags);
            //AddMessage('Flags: '+bod2tags+' '+IntToStr(bod2tags));
            if(bod2flags = $40000000) then begin
                // only the pip-boy flag is set
                // 0001110000000000000000000000001 -> first 3 are BODY, LHand, RHand

                Result := 'PipBoy';
                exit;
            end;
        end;

        if(hasNonPlayableFlag(e)) then begin
            exit;
        end;

        if(hasKeywordBySignature(e, 'VaultSuitKeyword', 'KWDA')) then begin
            Result := 'VaultSuit';
        end else if(hasKeywordBySignature(e, 'ArmorTypePower', 'KWDA')) then begin
            Result := 'PowerArmor';
        end else begin
            armorRating := 0;
            armorRatingStr := GetElementEditValues(e, 'FNAM\Armor Rating');
            if armorRatingStr <> '' then begin
                armorRating := StrToInt(armorRatingStr);
            end;
            if armorRating = 0 then begin
                Result := 'Clothing';
            end else begin
                Result := 'Armor';
            end;
        end;
    end;

    function getNoteType(e: IInterface): String;
    var
        curName, curModel: String;
    begin
        curName := DisplayName(e);

        if (hasScript(e, 'SimSettlements:Newspaper')) then begin
            Result := 'News';
            exit;
        end;

        if (hasScript(e, 'SimSettlementsV2:Books:NewsArticle')) then begin
            Result := 'News';
            exit;
        end;

        curModel := getModel(e);
        if(curModel = 'Props\NewspaperPublickOccurencesLowPoly.nif') then begin
            Result := 'News';
            exit;
        end;

        // script: CA_SkillMagazineScript
        if (hasKWDA(e, 'PerkMagKeyword') or hasScript(e, 'CA_SkillMagazineScript')) then begin
            Result := 'Perkmag';
            exit;
        end;

        Result := 'Note';
    end;

    function getWeapType(e: IInterface): String;
    var
        curName: String;
        ammo: IInterface;
        weapInrd: IInterface;
        newOverride: IInterface;
        flags: IInterface;
    begin
        Result := '';
        if(hasNonPlayableFlag(e)) then begin
            exit;
        end;

        // now only process explosives
        if hasKWDA(e, 'WeaponTypeExplosive') then begin
            // check if this has ammo
            ammo := getUsedAmmo(e);
            if (not assigned(ammo)) then begin
                // some explosive
                if hasKWDA(e, 'AnimsMine') then begin
                    Result := 'Mine';
                end else begin
                    Result := 'Grenade';
                end;
            end;
            exit;
        end;

        if(hasKWDA(e, 'WeaponTypeGrenade')) then begin
            Result := 'Grenade';
            exit;
        end;

        if(hasKWDA(e, 'WeaponTypeMine')) then begin
            Result := 'Mine';
            exit;
        end;

        // otherwise, this is a normal weapon
        weapInrd := LinksTo(ElementBySignature(e, 'INRD'));

        if(checkInnr(weapInrd)) then begin
            // nothing to do
            exit;
        end;

        newOverride := getOrCreateElementOverride(e, ToFile);

        AddMessage('Adding INNR to '+DisplayName(e));
        if(hasAnyKeyword(e, weaponTypesMelee, 'KWDA')) then begin
            // melee
            Result := 'Melee';
        end else begin
            // gun
            Result := 'Gun';
        end;
    end;

    function getAlchType(e: IInterface): String;
    var
        alchType: integer;
        tag: String;
        curName: String;
    begin
        alchType := getAlchemyType(e);

        tag := tagGoodChem;
        case (alchType) of
            1:  Result := 'GoodChem';
            2:  Result := 'BadChem';
            10: Result := 'Food'; // generic food, cooked
            11: Result := 'FoodRaw'; // raw
            12: Result := 'FoodPrewar'; // prewar
            13: Result := 'FoodCrop'; // crop
            20: Result := 'Drink';
            21: Result := 'Liquor';
            22: Result := 'Nukacola';
            30: Result := 'Syringe';
            40: Result := 'Device';
            41: Result := 'Tool'; //tools
        end;

    end;

    function getHolotapeType(e: IInterface): string;
    var
        holotapeType, curName, tagToUse, programName, edid: string;
    begin

        curName := DisplayName(e);
        holotapeType := getElementEditValues(e, 'DNAM');
        if(holotapeType = 'Sound') or (holotapeType = 'Voice') then begin
            Result := 'Holotape';
            exit;
        end;

        if (holotapeType = 'Program') then begin
            programName := getElementEditValues(e, 'PNAM');
            if(programName <> '') then begin
                programName := LowerCase(programName);
                if(gameProgramList.indexOf(programName) >= 0) then begin
                    Result := 'HolotapeGame';
                    exit;
                end;
            end;
        end;

        // Result := strStartsWithCI(fullString, list[index-1]);
        // special
        if(strStartsWith(curName, '- ')) then begin
            Result := 'HolotapeSettings';
            exit;
        end;

        if (pos('setting', curName) > 0 or pos('config', curName) > 0) then begin
            Result := 'HolotapeSettings';
            exit;
        end;

        edid := LowerCase(EditorID(e));

        if (pos('setting', edid) > 0 or pos('config', edid) > 0 or pos('cheat', edid) > 0) then begin
            Result := 'HolotapeSettings';
            exit;
        end;

        Result := 'Holotape';
    end;

    function getMiscType(e: IInterface): String;
    var
        value: integer;
        weight: float;
        numComponents: integer;
        i: integer;
        entry: IInterface;
        componentcontainer: IInterface;
        curName: String;
        components: String;
    begin
        // AddMessage('Processing misc');
        Result := '';
        curName := DisplayName(e);

        // test pipboy
        if(hasScript(e, 'DLC06:PipboyMiscItemScript')) or (hasScript(e, 'CreationsByCOOTS:PipboyMiscItemScript')) then begin
            Result := 'PipBoy';
            exit;
        end;

        if(hasScript(e, 'praVRF:SimulationData')) then begin
            Result := 'Holotape';
            exit;
        end;

        // it can be {Scrap}
        weight := StrToFloat( GetElementEditValues(e, 'DATA\Weight') );
        value := StrToInt( GetElementEditValues(e, 'DATA\Value') );

        components := getComponents(e);

        if components <> '' then begin
            // hack for gears, mostly
            if (curName = components) or (curName = components+'s') then begin
                // resource
                Result := 'Resource';
                exit;
            end;
            // scrap
            // exception for shipments
            // ShipmentScript
            if hasScript(e, 'ShipmentScript') then begin
                Result := 'Shipment';
                exit;
            end;

            Result := 'Scrap';
            exit;

        end;

        if isLooseMod(e) then begin
            Result := 'LooseMod';
            exit;
        end;

        if hasKWDA(e, 'FeaturedItem') then begin
            Result := 'Collectible';
            exit;
        end;

        if hasKWDA(e, 'VendorItemNoSale') or hasKWDA(e, 'UnscrappableObject') then begin
            Result := 'Quest';
            exit;
        end;

        if(LowerCase(getModel(e)) = 'props\pipboymiscitem\pipboymisc01.nif') then begin
            Result := 'PipBoy';
            exit;
        end;


        // START
        if ((weight = 0) and (value > 0)) then begin
            // weightless but valuable -> currency
            Result := 'Currency';
        end else if (value > 0) and (value >= weight) then begin
            // has weight and valuable -> valuable
            Result := 'Valuable';
        end else begin
            // otherwise trash
            Result := 'OtherMisc';
        end;
        // END


    end;

    function getItemTypeString(e: IInterface): string;
    var
        sig: string;
    begin
        Result := getTypeFromConfig(EditorID(e));
        if(Result <> '') then exit;

        sig := Signature(e);
        Result := '';

        if sig = 'KEYM' then begin
            Result := 'Key';
            exit;
        end else if sig = 'AMMO' then begin
            if(not hasNonPlayableFlag(e)) then begin
                Result := 'Ammo';
                exit;
            end;
        end else if sig = 'ARMO' then begin
            Result := getArmoType(e);
            exit;
        end else if sig = 'BOOK' then begin
            Result := getNoteType(e);
            exit;
        end else if sig = 'WEAP' then begin
            Result := getWeapType(e);
            exit;
        end else if sig = 'ALCH' then begin
            Result := getAlchType(e);
            exit;
        end else if sig = 'NOTE' then begin
            Result := getHolotapeType(e);
            exit;
        end else if(sig = 'MISC') then begin
            Result := getMiscType(e);
            exit;
        end;

    end;


    function jsonArrayContains(needle: String; haystack: TJsonArray): boolean;
    var
        i: integer;
    begin
        //AddMessage('searching in array for '+needle);
        for i:=0 to haystack.count-1 do begin
            //AddMessage(' checking '+haystack.S[i]);
            if(needle = haystack.S[i]) then begin
                //AddMessage(' found');
                Result := true;
                exit;
            end;
        end;
        Result := false;
    end;

    function checkItemTagForCfg(text: String; extraValidTags: TJsonArray): boolean;
    var
        c, tag, tagBare, tagNoBrackets: string;
        i, len: integer;
    begin
        // AddMessage('checkItemTagForCfg for '+text);
        Result := false;
        len := length(text);
        if(len = 0) then begin
            exit;
        end;
        // regex := '^([\[\]\(\)\{\}|][^\[\]\(\)\{\}|]+[\[\]\(\)\{\}|]).+';
        tag := regexExtract(text, tagExtractionRegex, 1);
        // AddMessage('checkItemTagForCfg got tag '+tag);
        if(tag = '') then begin
            exit;
        end;


        // for where the entire name is in brackets
        if(tag = text) then begin
            exit;
        end;

        tagBare := extractBareTag(text);
        //AddMessage('checkItemTagForCfg got bare tag '+tagBare);

        Result := (jsonArrayContains(tagBare, extraValidTags));
    end;

    procedure processForTaggingCfg(e: IInterface; targetFile: IInterface; typeStr, confName: string);
    var
        curData, curTagData: TJsonObject;
        extraValidTags: TJsonArray;
        prevName, curName, sig, cmpString, tag, newName, newNameBase: string;
        existingOverride, newOverride, checkElem: IInterface;
        newInrd, newInnr: IInterface;
        dryRunMode: boolean;
    begin
        dryRunMode := patcherConfig.B['dryRunMode'];
        AddMessage('Processing '+DisplayName(e)+' for '+confName+' into '+GetFileName(targetFile)+'; type='+typeStr);

        existingOverride := getExistingElementOverride(e, targetFile);
        checkElem := e;
        if(assigned(existingOverride)) then begin
            // checkElem := existingOverride;
            newOverride := existingOverride;
        end;

        curData := taggingConfigData.O[confName];
        //extraValidTags := curData.A['extraValidTags'];
        curTagData := curData.O['tags'];

        tag := curTagData.S[typeStr];

        curName := DisplayName(checkElem);

        if(curName = '') then begin
            AddMessage('elem has no name?');
            exit;
        end;

        if(patcherConfig.B['keepValidTags']) then begin
            if(checkItemTagForCfg(curName, curData.A['extraValidTags'])) then begin
                exit;
            end;
        end;// else begin
            newName := trim(regexExtract(curName, tagStripRegex, 1));
            if(newName = '') then begin
                newName := curName;
            end;
        //end;



        if(not assigned(newOverride)) then begin
            if(not dryRunMode) then begin
                newOverride := createElementOverride(e, targetFile);
            end;
        end else begin
            if(patcherConfig.B['keepValidTags']) then begin
                prevName := DisplayName(newOverride);

                if(checkItemTagForCfg(prevName, curData.A['extraValidTags'])) then begin
                    exit;
                end;
            end;

        end;

        sig := Signature(e);
        if (sig = 'MISC') or (sig = 'OMOD') then begin
            if(tag = '') then exit;


            newNameBase := removeComponentString(newName);

            cmpString := getComponents(checkElem);
            if (curData.O['settings'].B['addComponentString']) and (cmpString <> '') then begin
                newName := prefixNameWithTag(tag, newNameBase+'{{{'+cmpString+'}}}');
            end else begin
                newName := prefixNameWithTag(tag, newNameBase);
            end;

            if(not dryRunMode) then begin
                SetElementEditValues(newOverride, 'FULL', newName);
            end else begin
                AddMessage('Would be setting newName: '+newName);
            end;

            exit;
        end else if(sig = 'ARMO') then begin
            if(tag = '') then begin
                // otherwise special ARMO stuff

                if(typeStr = 'Armor') then begin
                    newInnr := innrCommonArmor;
                end else if(typeStr = 'PowerArmor') then begin
                    newInnr := innrPowerArmor;
                end else begin
                    newInnr := innrClothes;
                end;

                if(not dryRunMode) then begin
                    AddMessage('Adding INNR to '+DisplayName(newOverride));
                    setPathLinksTo(newOverride, 'INRD', newInnr);
                    ensureDefaultObjectTemplate(newOverride);
                end else begin
                    AddMessage('Would be Adding INNR '+DisplayName(newInnr)+' to '+DisplayName(newOverride));
                end;

                exit;
            end;
        end else if(sig = 'WEAP') then begin
            if(tag = '') then begin
                newInnr := innrCommonGun;
                if(typeStr = 'Melee') then begin
                    newInnr := innrCommonMelee;
                end;
                if(not dryRunMode) then begin
                    AddMessage('Adding INNR to '+DisplayName(newOverride));
                    setPathLinksTo(newOverride, 'INRD', newInnr);

                    ensureDefaultObjectTemplate(newOverride);
                end else begin
                    AddMessage('Would be Adding INNR '+DisplayName(newInnr)+' to '+DisplayName(newOverride));
                end;
                exit;
            end;
        end;

        newName := prefixNameWithTag(tag, newName);
        if(not dryRunMode) then begin
            SetElementEditValues(newOverride, 'FULL', newName);
        end else begin
            AddMessage('Would be setting new name: '+newName);
        end;
        exit;
    end;

    procedure multiTagifyElement(e: IInterface);
    var
      curSig, newName, curName, curFileName: String;
      scrapComponentsString: String;
      newElem, curOverride, existingOverride, winOverride, masterElem, curFile: IInterface;
      curEdid, typeStr, taggingName: String;
      i, numOverrides: integer;
    begin
        curSig := Signature(e);

        if (sigsToProcess.indexOf(curSig) < 0) then begin
            // AddMessage('Skipping, signature');
            exit;
        end;

        curEdid := GetElementEditValues(e, 'EDID');
        if(isEdidIgnored(curEdid)) then begin
            // AddMessage('Skipping '+curName+', EDID');
            exit;
        end;

        if (
            hasAnyScript(e, scriptBlackList) or
            hasAnyScriptInPrefixList(e, scriptBlackListPrefix)
        ) then begin
            // AddMessage('Skipping '+curName+', Script');
            exit;
        end;
        // AddMessage('Script ok');

        curName := DisplayName(e);
        if(curName = '') then begin
            exit;
        end;

        // fish out winning override, unless in any of the patches
        masterElem   := MasterOrSelf(e);
        numOverrides := OverrideCount(masterElem);
        for i:= 0 to numOverrides-1 do begin
            curOverride := OverrideByIndex(masterElem, i);
            curFileName := GetFileName(GetFile(curOverride));

            if(fileMap.indexOf(curFileName) < 0) then begin
                winOverride := curOverride;
            end;
        end;

        if(not assigned(winOverride)) then begin
            winOverride := e;
        end;

        curName := DisplayName(winOverride);

        if(curName = '') then begin
            exit;
        end;

        // find type
        typeStr := getItemTypeString(e);
        if(typeStr = '') then exit;

        AddMessage('Item Type: '+EditorID(e)+'='+typeStr);

        // now apply it for each file
        for i:=0 to fileMap.count-1 do begin
            taggingName := fileMap[i];
            curFile := ObjectToElement(fileMap.Objects[i]);
            processForTaggingCfg(e, curFile, typeStr, taggingName);
        end;
    end;

    function Process(e: IInterface): integer;
    var
      newElem: IInterface;
    begin
        if Signature(e) = 'TES4' then exit;

        multiTagifyElement(e);
    end;

    function Finalize: integer;
    var
        i: integer;
        taggingName: string;
        curFile: IInterface;
    begin
        for i:=0 to fileMap.count-1 do begin
            taggingName := fileMap[i];
            curFile := ObjectToElement(fileMap.Objects[i]);
            CleanMasters(curFile);
        end;

        Result := 0;
        cleanupTagifier();
        taggingConfigData.free();
        fileMap.free();
        typeConfigKeys.free();
        typeConfigValues.free();
        patcherConfig.free();
    end;

end.