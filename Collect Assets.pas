{
    Collect all the assets!
}
unit CollectAssets;
    uses PraUtil, PexToJson;

    const
        configFile = ScriptsPath + 'Collect Assets.cfg';

    var
        // these are ALL resources, to prevent double processing
        scriptNames: TStringList; // raw script names!

        nifNames: TStringList; // includes meshes\
        matNames: TStringList; // includes materials\
        texNames: TStringList; // includes textures\
        sndNames: TStringList; // includes sound\

        resourceNames: TStringList; // all resources which are to be packed

        resourceBlackList: TStringList;

        existingResourceList: TStringList; // all resources in all currently loaded archives
        missingResourceList: TStringList; // missing ones go here

        // Config Stuff
        verboseMode: boolean;
        addSource: boolean;
        deleteDirectoryAfterwards: boolean;
        addFacemesh: boolean;
        addScolMesh: boolean;
        useXwm: boolean;
        parseScripts: boolean;
        useResourceBlacklist: boolean;
        addAnimFolder: boolean;
        addLod: boolean;
        processNifs: boolean;

        extractExistingBa2: boolean;
        packNewBa2: boolean;
        showListOfAssets: boolean;
        showMissingAssets: boolean;

        currentFilename: string;
        outputDir: string;
        archive2path: string;

        hasTextureOutput: boolean;
        hasMainOutput: boolean;

        shouldCompressMainBa2: boolean;


    procedure loadConfig();
    var
        i, j, breakPos: integer;
        curLine, curKey, curVal: string;
        lines : TStringList;
    begin
        // default
        addSource := true;
        addFacemesh := true;
        addScolMesh := false;
        useXwm := true;
        parseScripts := true;
        useResourceBlacklist := true;
        addAnimFolder := true;
        processNifs := true;
        addLod := true;

        verboseMode := false;
        showListOfAssets := true;
        showMissingAssets := true;

        deleteDirectoryAfterwards := true;
        extractExistingBa2 := true;
        packNewBa2 := true;
        shouldCompressMainBa2 := true;

        if(not FileExists(configFile)) then begin
            exit;
        end;
        lines := TStringList.create;
        lines.LoadFromFile(configFile);

        //
        for i:=0 to lines.count-1 do begin
            curLine := lines[i];
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

                if(curKey = 'addSource') then begin
                    addSource := StrToBool(curVal);
                end else if(curKey = 'addFacemesh') then begin
                    addFacemesh := StrToBool(curVal);
                end else if(curKey = 'addScolMesh') then begin
                    addScolMesh := StrToBool(curVal);
                end else if(curKey = 'useXwm') then begin
                    useXwm := StrToBool(curVal);
                end else if(curKey = 'parseScripts') then begin
                    parseScripts := StrToBool(curVal);
                end else if(curKey = 'useResourceBlacklist') then begin
                    useResourceBlacklist := StrToBool(curVal);
                end else if(curKey = 'addAnimFolder') then begin
                    addAnimFolder := StrToBool(curVal);
                end else if(curKey = 'processNifs') then begin
                    processNifs := StrToBool(curVal);
                end else if(curKey = 'verboseMode') then begin
                    verboseMode := StrToBool(curVal);
                end else if(curKey = 'showListOfAssets') then begin
                    showListOfAssets := StrToBool(curVal);
                end else if(curKey = 'showMissingAssets') then begin
                    showMissingAssets := StrToBool(curVal);
                end else if(curKey = 'deleteDirectoryAfterwards') then begin
                    deleteDirectoryAfterwards := StrToBool(curVal);
                end else if(curKey = 'extractExistingBa2') then begin
                    extractExistingBa2 := StrToBool(curVal);
                end else if(curKey = 'packNewBa2') then begin
                    packNewBa2 := StrToBool(curVal);
                end else if(curKey = 'shouldCompressMainBa2') then begin
                    shouldCompressMainBa2 := StrToBool(curVal);
                end;
            end;
        end;

        lines.free();
    end;

    procedure saveConfig();
    var
        lines : TStringList;
    begin
        lines := TStringList.create;


        lines.add('addSource='+BoolToStr(addSource));
        lines.add('addFacemesh='+BoolToStr(addFacemesh));
        lines.add('addScolMesh='+BoolToStr(addScolMesh));
        lines.add('useXwm='+BoolToStr(useXwm));
        lines.add('parseScripts='+BoolToStr(parseScripts));
        lines.add('useResourceBlacklist='+BoolToStr(useResourceBlacklist));
        lines.add('addAnimFolder='+BoolToStr(addAnimFolder));
        lines.add('processNifs='+BoolToStr(processNifs));
        lines.add('addLod='+BoolToStr(addLod));
        lines.add('verboseMode='+BoolToStr(verboseMode));
        lines.add('showListOfAssets='+BoolToStr(showListOfAssets));
        lines.add('showMissingAssets='+BoolToStr(showMissingAssets));
        lines.add('deleteDirectoryAfterwards='+BoolToStr(deleteDirectoryAfterwards));
        lines.add('extractExistingBa2='+BoolToStr(extractExistingBa2));
        lines.add('packNewBa2='+BoolToStr(packNewBa2));
        lines.add('shouldCompressMainBa2='+BoolToStr(shouldCompressMainBa2));

        lines.saveToFile(configFile);
        lines.free();
    end;


    function ExtractFileBasename(filename: string): string;
    var
        curExt: string;
    begin
        curExt := ExtractFileExt(filename);

        Result := copy(filename, 0, length(filename)-length(curExt));
    end;

    function normalizeSlashes(name: string): string;
    begin
        Result := StringReplace(name, '/', '\', [rfReplaceAll]);
    end;

    function normalizeResource(path: string; akResourceType: TGameResourceType): string;
    begin
        Result := wbNormalizeResourceName(normalizeSlashes(path), akResourceType);
    end;

    function isResourceBlacklisted(resName: string): boolean;
    var
        i: integer;
        curRes: string;
    begin
        Result := false;
        if(resourceBlackList.indexOf(resName) >= 0) then begin
            Result := true;
            exit;
        end;

        for i:=0 to resourceBlackList.count-1 do begin
            curRes := resourceBlackList[i];
            if(curRes <> '') then begin
                if(strStartsWithCI(resName, curRes)) then begin
                    Result := true;
                    exit;
                end;
            end;
        end;
    end;

    procedure checkMissingResource(resName: string);
    begin
        if(not showMissingAssets) then exit;
        if(existingResourceList.indexOf(resName) < 0) then begin
            // this is a missing asset
            missingResourceList.add(resName);
        end;
    end;

    function processResource(resName: string): boolean;
    begin
        if(resName = '') then begin
            Result := false;
            exit;
        end;

        resName := normalizeSlashes(resName);

        if(verboseMode) then begin
            AddMessage('Checking: '+resName);
        end;

        checkMissingResource(resName);

        if (not fileExists(DataPath+resName)) then begin
            if(verboseMode) then begin
                AddMessage('Not a loose file: '+resName);
            end;
            Result := false;
            exit;
        end;
        if (resourceNames.indexOf(resName) >= 0) then begin
            if(verboseMode) then begin
                AddMessage('Already added: '+resName);
            end;
            Result := false;
            exit;
        end;

        if (useResourceBlacklist) then begin
                if(isResourceBlacklisted(resName)) then begin
                    if(verboseMode) then begin
                        AddMessage('Resource blacklisted: '+resName);
                    end;
                exit;
            end;
        end;

        if(verboseMode) then begin
            AddMessage('Found resource '+resName);
        end;
        resourceNames.add(resName);
        Result := true;
    end;


    function processResourceDirectoryRecursive(dir: string): boolean;
    var
        curFullPath: string;
        searchResult : TSearchRec;
        curFile: string;
    begin
        curFullPath := DataPath + stripSlash(dir);

        Result := false;

        if(not DirectoryExists(curFullPath)) then begin
            exit;
        end;

        if FindFirst(curFullPath+'\*', faAnyFile, searchResult) = 0 then begin
            repeat
                // ignore . and ..
                if(searchResult.Name <> '.') and (searchResult.Name <> '..') then begin
                    curFile := stripSlash(dir)+'\'+searchResult.Name;

                    if((searchResult.attr and faDirectory) = faDirectory) then begin
                        // dir
                        if(processResourceDirectoryRecursive(curFile)) then begin
                            Result := true;
                        end;
                    end else begin
                        // file
                        if(processResource(curFile)) then begin
                            Result := true;
                        end;
                    end;
                end;
            until FindNext(searchResult) <> 0;

            // Must free up resources used by these successful finds
            FindClose(searchResult);
        end;


    end;

    /////////////// SCRIPTS BEGIN ////////////////
    {
        converts a script name to the path of the corresponding PSC file
    }
    function scriptNameToSourcePath(name: string): string;
    begin
        Result := StringReplace(name, ':', '\', [rfReplaceAll]);
        // Result := LowerCase(StringReplace(name, ':', '\', [rfReplaceAll]));
        Result := 'scripts\source\user\' + Result + '.psc';
    end;

    {
        converts a script name to the path of the corresponding PEX file
    }
    function scriptNameToPexPath(name: string): string;
    begin
        // Result := LowerCase(StringReplace(name, ':', '\', [rfReplaceAll]));
        Result := StringReplace(name, ':', '\', [rfReplaceAll]);
        Result := 'scripts\' + Result + '.pex';
    end;

    procedure decompileScript(pexPath: string);
    var
        pexData, curObj: TJsonObject;
        i: integer;
        outList: TStringList;
        scriptName: string;
    begin
        if(verboseMode) then begin
            AddMessage('Decompiling '+pexPath);
        end;
        pexData := readPexResource(pexPath);
        if(pexData <> nil) then begin
            outList := getUsedScripts(pexData);
            for i:=0 to outList.count-1 do begin
                scriptName := outList[i];
                processScriptName(scriptName);
            end;

            outList.free();
            pexData.free();
        end else begin
            if(verboseMode) then begin
                AddMessage('  Failed to decompile!');
            end;
        end;
    end;

    procedure processScriptName(scriptName: string);
    var
        sourcePath: string;
        destDir: string;
        pscPath, pexPath: string;
    begin
        if(scriptNames.indexOf(scriptName) < 0) then begin
            scriptNames.add(scriptName);

            if(addSource) then begin
                processResource(scriptNameToSourcePath(scriptName));
            end;

            pexPath := scriptNameToPexPath(scriptName);

            if(processResource(pexPath)) then begin
                if(parseScripts) then begin
                    decompileScript(pexPath);
                end;
            end;
        end;
    end;

    procedure processScriptElem(script: IInterface);
    var
        curScriptName: string;
    begin
        curScriptName := GetElementEditValues(script, 'scriptName');
        if(curScriptName <> '') then begin
            processScriptName(curScriptName);
        end;
    end;

    procedure processScripts(e: IInterface);
    var
        vmad, scriptList, frags, aliases: IInterface;
        curScript, curAlias: IInterface;
        i, j: integer;
    begin
        vmad := ElementByName(e, 'VMAD - Virtual Machine Adapter');
        if(not assigned(vmad)) then begin
            exit;
        end;

        // scripts
        scriptList := ElementByName(vmad, 'Scripts');
        if(assigned(scriptList)) then begin
            for i := 0 to ElementCount(scriptList)-1 do begin
                curScript := ElementByIndex(scriptList, i);
                processScriptElem(curScript);
            end;
        end;

        // fragments
        frags := ElementByName(vmad, 'Script Fragments');
        if(assigned(frags)) then begin
            curScript := ElementByName(frags, 'Script');
            if(assigned(curScript)) then begin
                processScriptElem(curScript);
            end;

            // sometimes frags itself has a script
            processScriptElem(frags);
        end;

        // quest aliases
        aliases := ElementByName(vmad, 'Aliases');
        if(assigned(aliases)) then begin
            for i := 0 to ElementCount(aliases)-1 do begin
                curAlias := ElementByIndex(aliases, i);
                scriptList := ElementByName(curAlias, 'Alias Scripts');
                for j := 0 to ElementCount(scriptList)-1 do begin
                    curScript := ElementByIndex(scriptList, j);
                    processScriptElem(curScript);
                end;
            end;
        end;
    end;
    //////////////// SCRIPTS END /////////////////

    /////////////// TEXTURES BEGIN ///////////////



    procedure processTexture(textureName: String);
    begin
        if(textureName = '') then begin
            exit;
        end;

        textureName := normalizeResource(textureName, resTexture);

        if(texNames.indexOf(textureName) >= 0) then begin
            exit;
        end;

        texNames.add(textureName);

        processResource(textureName);
    end;

    procedure GetTexturesFromTextureSet(aSet: TwbNifBlock);
    var
        i: integer;
        el: TdfElement;
    begin
        if not Assigned(aSet) then
            Exit;

        el := aSet.Elements['Textures'];
        for i := 0 to Pred(el.Count) do begin
            processTexture(el[i].EditValue);
        end;
    end;

	procedure processTextureIfEntryExists(block: TwbNifBlock; entryName: string);
	var
		curValue: string;
	begin
		curValue := block.EditValues[entryName];

		if (curValue <> '') then begin
			processTexture(curValue);
		end;
	end;

    procedure processMaterial(materialName: string);
    var
        matFile: TdfStruct;

        i: integer;
        el: TdfElement;
    begin
        if(materialName = '') then begin
            exit;
        end;

        if(matNames.indexOf(materialName) >= 0)  then begin
            exit;
        end;

        checkMissingResource(materialName);
        if(not FileExists(DataPath+materialName)) then begin
            exit;
        end;

        materialName := normalizeSlashes(materialName);

        matNames.add(materialName);
        processResource(materialName);

        if SameText(ExtractFileExt(materialName), '.bgsm') then begin
            matFile := TwbBGSMFile.Create;
        end else if SameText(ExtractFileExt(materialName), '.bgem') then begin
            matFile := TwbBGEMFile.Create;
        end;

        matFile.LoadFromResource(materialName);
        el := matFile.Elements['Textures'];
        if Assigned(el) then begin
            for i := 0 to Pred(el.Count) do begin
                processTexture(el[i].EditValue);
            end;
        end;
        matFile.Free;
    end;
    //////////////// TEXTURES END ////////////////


    //////////////// MODELS BEGIN ////////////////
    function getModelNameByPath(e: IInterface; path: string): string;
    var
        tempElem: IInterface;
    begin
        Result := '';
        tempElem := ElementByPath(e, path);
        if(assigned(tempElem)) then begin
            Result := GetEditValue(tempElem);
        end;
    end;

    function getModelName(e: IInterface): string;
    var
        tempElem: IInterface;
    begin
        Result := getModelNameByPath(e, 'Model\MODL');
    end;

    function getMaterialSwap(e: IInterface): IInterface;
    var
        tempElem: IInterface;
    begin
        Result := nil;
        tempElem := ElementByName(e, 'Model');
        if(assigned(tempElem)) then begin
            // ALSO MATERIAL SWAP
            tempElem := ElementBySignature(tempElem, 'MODS');
            if(assigned(tempElem)) then begin
                Result := LinksTo(tempElem);
            end;
        end;
    end;



    procedure processNif(nifPath: string);
    var
        i: integer;
        nif: TwbNifFile;
        Block: TwbNifBlock;
        curBlockName: string;
        hasMaterial: boolean;
    begin
        if(not processNifs) then exit;
        nif := TwbNifFile.Create;
        // mostly stolen from kinggath...

        try
            nif.LoadFromResource(nifPath);

            for i := 0 to Pred(Nif.BlocksCount) do begin
                Block := Nif.Blocks[i];

                if (Block.BlockType = 'BSLightingShaderProperty') or (Block.BlockType = 'BSEffectShaderProperty') then begin
                    // check for material file in the Name field of FO4 meshes
                    hasMaterial := False;
                    if nif.NifVersion = nfFO4 then begin
                        // if shader material is used, get textures from it
                        // at this point, s is the material name
                        curBlockName := Block.EditValues['Name'];
                        if (SameText(ExtractFileExt(curBlockName), '.bgsm') or SameText(ExtractFileExt(curBlockName), '.bgem')) then begin
                            curBlockName := normalizeResource(curBlockName, resMaterial);

                            processMaterial(curBlockName);
                            hasMaterial := True;
                        end;

                        curBlockName := Block.EditValues['Wet Material'];
                        if (SameText(ExtractFileExt(curBlockName), '.bgsm') or SameText(ExtractFileExt(curBlockName), '.bgem')) then begin
                            curBlockName := normalizeResource(curBlockName, resMaterial);

                            processMaterial(curBlockName);
                        end;
                    end;
                    // no material used, get textures from texture set
                    if not hasMaterial then begin
                        if(assigned(Block.Elements['Texture Set'])) then begin
                            GetTexturesFromTextureSet(Block.Elements['Texture Set'].LinksTo);
                        end;
                    end;

					// special stuff
					processTextureIfEntryExists(Block, 'Source Texture');
					processTextureIfEntryExists(Block, 'Greyscale Texture');
					processTextureIfEntryExists(Block, 'Env Map Texture');
					processTextureIfEntryExists(Block, 'Normal Texture');
					processTextureIfEntryExists(Block, 'Env Mask Texture');

                end else if(Block.BlockType = 'BSBehaviorGraphExtraData') then begin
                    curBlockName := Block.EditValues['Behavior Graph File'];
                    if(curBlockName <> '') then begin
                        ProcessResource(wbNormalizeResourceName(curBlockName, resMesh));
                    end;
                end else if(Block.BlockType = 'BSShaderPPLightingProperty') then begin
                    if(assigned(Block.Elements['Texture Set'])) then begin
                        GetTexturesFromTextureSet(Block.Elements['Texture Set'].LinksTo);
                    end;
                end else if(Block.BlockType = 'BSShaderNoLightingProperty') or
                            (Block.BlockType = 'TallGrassShaderProperty') or
                            (Block.BlockType = 'TileShaderProperty') or
                            Block.IsNiObject('NiTexture', True) then begin

                    curBlockName := Block.EditValues['File Name'];
                    if(curBlockName <> '') then begin
                        ProcessResource(wbNormalizeResourceName(curBlockName, resTexture));
                    end;
                end else if(Block.BlockType = 'BSSkyShaderProperty') then begin
                    curBlockName := Block.EditValues['Source Name'];
                    if(curBlockName <> '') then begin
                        ProcessResource(wbNormalizeResourceName(curBlockName, resTexture));
                    end;
                end else if(Block.BlockType = 'BSSubIndexTriShape') then begin
                    curBlockName := Block.EditValues['Segment Data\SSF File'];
                    if(curBlockName <> '') then begin
                        ProcessResource(wbNormalizeResourceName(curBlockName, resMesh));
                    end;
                end;

            end;
        finally
            nif.free();
        end;
    end;

    procedure processModel(modelName: string);
    var
        modelNameFull: string;
    begin
        //modelName := 'meshes\' + LowerCase(modelName);
        //modelName := 'Meshes\' + normalizeSlashes(modelName);
        modelName := wbNormalizeResourceName(modelName, resMesh);

        if(nifNames.indexOf(modelName) >= 0) then begin
            // done that already
            exit;
        end;

        modelNameFull := DataPath+modelName;

        checkMissingResource(modelName);
        if(not fileExists(modelNameFull)) then begin
            exit;
        end;
        nifNames.add(modelName);
        processResource(modelName);

        processNif(modelName);
    end;

    procedure processMatSwap(e: IInterface);
    var
        subs, curSub: IInterface;
        i, numElems: integer;
        replacementMat: string;
    begin
        subs := ElementByName(e, 'Material Substitutions');
        if(not assigned(subs)) then begin
            exit;
        end;

        numElems := ElementCount(subs);

        for i := 0 to numElems-1 do begin
            curSub := ElementByIndex(subs, i);
            //dumpElement(curSub, '');
            // bnam : original
            // snam : replacement
            replacementMat := GetElementEditValues(curSub, 'SNAM - Replacement Material');
            // addMessage('Found mat: '+replacementMat);
            processMaterial(normalizeResource(replacementMat, resMaterial));
        end;
    end;

    procedure processModelByPath(e: IInterface; path: string);
    var
        modelName: string;
        matSwap: IInterface;
    begin

        modelName := getModelNameByPath(e, path);
        if(modelName <> '') then begin
            processModel(modelName);
            matSwap := getMaterialSwap(e);

            if(assigned(matSwap)) then begin
                processMatSwap(matSwap);
            end;
        end;
    end;

    procedure processModels(e: IInterface);
    var
        modelName: string;
        matSwap: IInterface;
    begin
        if(Signature(e) = 'SCOL') and (not addScolMesh) then begin
            exit;
        end;

        processModelByPath(e, 'Model\MODL');
        processModelByPath(e, '1st Person Model\MOD4');
        // for ARMO
        processModelByPath(e, 'Male world model\MOD2');
        processModelByPath(e, 'Female world model\MOD3');

        processModelByPath(e, 'Male 1st Person\MOD4');
        processModelByPath(e, 'Female 1st Person\MOD5');

    end;

    procedure processPrevisParent(e: IInterface);
    var
        masterName, formIdHex, previsFileName: string;
        cellMaster: IInterface;
    begin
        if(not assigned(e)) then begin
            exit;
        end;
        cellMaster := MasterOrSelf(e);
        formIdHex := IntToHex(getElementLocalFormId(cellMaster) and $00FFFFFF, 8);

        masterName := GetFileName(GetFile(cellMaster));


        previsFileName := 'Vis\'+masterName+'\'+formIdHex+'.uvd';
        //AddMessage('Vis filename: '+previsFileName);
        processResource(previsFileName);
    end;

    procedure processPhysicsMesh(e: IInterface);
    var
        meshName, formIdHex, masterName: string;
        cellMaster: IInterface;
    begin
        cellMaster := MasterOrSelf(e);

        masterName := GetFileName(GetFile(cellMaster));
        formIdHex := IntToHex(getElementLocalFormId(cellMaster) and $00FFFFFF, 8);// hmm, what about ESLs?

        // if masterName is Fallout4.esm, then we don't have the masterName part??
        if(LowerCase(masterName) = 'fallout4.esm') then begin
            meshName := 'Meshes\PreCombined\'+formIdHex+'_Physics.NIF';
        end else begin
            meshName := 'Meshes\PreCombined\'+masterName+'\'+formIdHex+'_Physics.NIF';
        end;
        //AddMessage('PHYSICS '+meshName);
        processResource(meshName);
    end;


    procedure processPrecombines(e: IInterface);
    var
        xcri, rvis, rvisTarget, meshes: IInterface;
        meshName: string;
        i: integer;
    begin
        xcri := ElementBySignature(e, 'XCRI');
        if(assigned(xcri)) then begin
            meshes :=  ElementByName(xcri, 'Meshes');

            for i:=0 to ElementCount(meshes)-1 do begin
                meshName := GetEditValue(ElementByIndex(meshes, i));
                processModel(meshName);
            end;
        end;

        // TODO: try to figure out if we even have previs and physics

        // now previs
        rvis := ElementBySignature(e, 'RVIS');
        if(assigned(rvis)) then begin
            rvisTarget := LinksTo(rvis);
            processPrevisParent(rvisTarget);
            //dumpElem(rvisTarget);
        end else begin
            // maybe this file exists anyway
            processPrevisParent(e);
        end;

        // now physics.
        processPhysicsMesh(e);
    end;

    procedure processNpcModels(e: IInterface);
    var
        modelName, modelNameFull, curNameBase, curNameExt: string;
        formId: LongWord;
        templateElem: IInterface;
    begin
        // so apparently, the face meshes are used if Traits are NOT enabled
        templateElem := ElementBySignature(e, 'ACBS');
        if(assigned(templateElem)) then begin
            templateElem := ElementByName(templateElem, 'Use Template Actors');
            if(assigned(templateElem)) then begin
                // check if "Traits" is set
                if(GetElementEditValues(templateElem, 'Traits') = '1') then begin
                    // we have the trait. so nothing to do.
                    exit;
                end;
            end;
        end;

        formId := GetLoadOrderFormID(e);
        modelName := IntToHex(formId and 16777215, 8) + '.nif';

        curNameBase := ExtractFileBasename(currentFilename);
        curNameExt := ExtractFileExt(currentFilename);

        processModel('Actors\Character\FaceGenData\FaceGeom\'+curNameBase+curNameExt+'\' + modelName);

        if(SameText(curNameExt, '.esl')) then begin
            // try this too
            processModel('Actors\Character\FaceGenData\FaceGeom\'+curNameBase+'.esp\' + modelName);
        end;
    end;

    ///////////////// MODELS END /////////////////


    //////////////// SOUNDS BEGIN ////////////////
    procedure dumpElement(e: IInterface; prefix: String);
    var
        i: Integer;
        child: IInterface;
    begin
        for i := 0 to ElementCount(e)-1 do begin
            child := ElementByIndex(e, i);
            AddMessage(prefix+DisplayName(child)+'='+GetEditValue(child));
            dumpElement(child, prefix+'  ');
        end
    end;

    procedure processSound(sndName: string);
    var
        xwmName: string;
    begin
        sndName := normalizeSlashes(sndName);
        if(sndNames.indexOf(sndName) >= 0) then begin
            exit;
        end;

        sndNames.add(sndName);
        if(useXwm and SameText(ExtractFileExt(sndName), '.wav')) then begin
            xwmName := ExtractFileBasename(sndName) + '.xwm';

            if(processResource(xwmName)) then begin
                // done
                exit;
            end;
        end;

        processResource(sndName);

        //AddMessage('Sound '+sndName);
    end;

    procedure processSounds(e: IInterface);
    var
        soundList, curSnd: IInterface;
        i: Integer;
        curSndFilename: string;
    begin
        //dumpElement(e, '');
        soundList := ElementByName(e, 'Sounds');

        //dumpElement(soundList, '');
        if(assigned(soundList)) then begin
            for i := 0 to ElementCount(soundList)-1 do begin
                curSnd := ElementByIndex(soundList, i);

                curSndFilename := GetEditValue( ElementBySignature(curSnd, 'ANAM'));

                // dumpElement(curSnd, '');
                processSound(normalizeResource(curSndFilename, resSound));
            end;
        end;
{
        // it could also have ANAM right away
        soundList := ElementBySignature(e, 'ANAM');
        if(assigned(soundList)) then begin
            curSndFilename := GetEditValue(soundList);
            processSound(normalizeResource(curSndFilename, resSound));
        end;
}

    end;

    procedure processRace(e: IInterface);
    var
        subGraph, animPaths, curData: IInterface;
        i, j: integer;
        curHkx: string;
    begin
        subGraph := ElementByPath(e, 'Subgraph Data');

        if(not assigned(subGraph)) then exit;

        for i:=0 to ElementCount(subGraph)-1 do begin
            curData := ElementByIndex(subGraph, i);
            curHkx := GetElementEditValues(curData, 'SGNM');
            // AddMessage('MAIN HKX '+curHkx);
            if(curHkx <> '') then begin
                //processResource('meshes\'+LowerCase(curHkx));
                // processResource('Meshes\'+curHkx);
                processResource(wbNormalizeResourceName(curHkx, resMesh));
            end;

            animPaths := ElementByPath(curData, 'Animation Paths');


            for j:=0 to ElementCount(animPaths)-j do begin
                curHkx := GetEditValue(ElementByIndex(animPaths, j));
                if(curHkx <> '') then begin
                    //processResourceDirectoryRecursive('meshes\'+LowerCase(curHkx));
                    processResourceDirectoryRecursive('Meshes\'+curHkx);
                    // AddMessage('SUB HKX '+curHkx);
                end;
            end;
        end;
    end;

    procedure processVoiceType(e: IInterface);
    var
        dialFormId: cardinal;
        formIdStr, basePath, curFormId, curExt: string;
        responses, curRsp, curFile, curDial: IInterface;
        i: integer;
        searchResult : TSearchRec;
    begin
        basePath := 'Sound\Voice\'+GetFileName(GetFile(e)) + '\' +EditorID(e);
        if(not DirectoryExists(DataPath + basePath)) then begin
            // AddMessage('nope');
            exit;
        end;

        curFile := GetFile(e);

        if FindFirst(DataPath+basePath+'\*', faAnyFile, searchResult) = 0 then begin
            repeat
                // ignore . and ..
                if(searchResult.Name <> '.') and (searchResult.Name <> '..') then begin

                    try
                        curExt := ExtractFileExt(searchResult.Name); // must be .fuz (or .wav or .xwm ???)
                        curFormId := copy(searchResult.Name, 0, 8);
                        dialFormId := StrToInt('$' + curFormId);
                        curDial := getFormByFileAndFormID(curFile, dialFormId);
                        if assigned(curDial) then begin
                            processSound(normalizeResource(basePath+'\'+searchResult.Name, resSound));
                            //AddMessage('yes '+IntToHex(dialFormId, 8));
                        end;
                    except
                        AddMessage('File '+searchResult.Name+' seems to be invalid');
                    end;
                end;
            until FindNext(searchResult) <> 0;

            // Must free up resources used by these successful finds
            FindClose(searchResult);
        end;
    end;
    ///////////////// SOUNDS END /////////////////


    ////////////////// GUI BEGIN /////////////////


    function showConfigGui(): boolean;
    var
        frm: TForm;
        btnOkay, btnCancel: TButton;
        windowHeightBase: Integer;
        windowWidthBase: Integer;
        topOffset: Integer;

        cbIncludeSource: TCheckBox;
        //cbIgnoreNamespaceless: TCheckBox;
        includeFaceMeshes: TCheckBox;
        includeScol: TCheckBox;
        cbAutoXwm: TCheckBox;


        processingGroup: TGroupBox;

        outputGroup: TGroupBox;
        unpackExisting: TCheckBox;
        repackOutput: TCheckBox;
        deleteDirectory: TCheckBox;
        parseScriptsCb: TCheckBox;
        blacklistScriptsCb: TCheckBox;
        compressMainBa2: TCheckBox;
        addAnimFolderCb: TCheckBox;
        processNifsCb: TCheckBox;
        addLodCb: TCheckBox;

        interfaceGroup: TGroupBox;
        verboseModeCb: TCheckBox;
        showListOfAssetsCb: TCheckBox;
        showMissingAssetsCb: TCheckBox;

        resultCode: Integer;
    begin
        loadConfig();
        scriptNames := TStringList.create;
        nifNames := TStringList.create;
        matNames := TStringList.create;
        texNames := TStringList.create;
        sndNames := TStringList.create;
        resourceNames := TStringList.create;

        scriptNames.CaseSensitive := false;
        nifNames.CaseSensitive := false;
        matNames.CaseSensitive := false;
        texNames.CaseSensitive := false;
        sndNames.CaseSensitive := false;
        resourceNames.CaseSensitive := false;

        Result := true;

        windowHeightBase := 400;
        windowWidthBase := 360;
        topOffset := 0;

        frm := CreateDialog('Asset Collector', windowWidthBase+10, windowHeightBase+70);

        btnOkay := CreateButton(frm, 10, windowHeightBase, 'OK');
        btnOkay.ModalResult := mrYes;
        btnOkay.width := 75;

        btnCancel := CreateButton(frm, 90, windowHeightBase, 'Cancel');
        btnCancel.ModalResult := mrCancel;
        btnCancel.width := 75;

        topOffset := 5;
        interfaceGroup := CreateGroup(frm, 10, topOffset, 345, 70, 'Interface');
        showListOfAssetsCb  := CreateCheckbox(interfaceGroup, 10, 15,'Show asset dialog after processing');
        if(showListOfAssets) then begin
            showListOfAssetsCb.state := cbChecked;
        end;
        showMissingAssetsCb := CreateCheckbox(interfaceGroup, 10, 35,'Output missing assets');
        if(showMissingAssets) then begin
            showMissingAssetsCb.state := cbChecked;
        end;
        verboseModeCb       := CreateCheckbox(interfaceGroup, 10, 55,'Verbose mode (more info outputs)');
        if(verboseMode) then begin
            verboseModeCb.state := cbChecked;
        end;

        topOffset := topOffset + 80;
        processingGroup := CreateGroup(frm, 10, topOffset, 345, 190, 'Processing');

        cbIncludeSource         := CreateCheckbox(processingGroup, 10, 15,'Include Script Sources');
        if(addSource) then begin
            cbIncludeSource.state := cbChecked;
        end;
        includeFaceMeshes       := CreateCheckbox(processingGroup, 10, 35,'Include Face Meshes');
        if(addFacemesh) then begin
            includeFaceMeshes.state := cbChecked;
        end;

        includeScol := CreateCheckbox(processingGroup, 10, 55, 'Include SCOL Meshes');
        if(addScolMesh) then begin
            includeScol.state := cbChecked;
        end;

        cbAutoXwm       := CreateCheckbox(processingGroup, 10, 75,'Use XWM sound files, if possible');
        if(useXwm) then begin
            cbAutoXwm.State := useXwm;
        end;

        parseScriptsCb       := CreateCheckbox(processingGroup, 10, 95,'Parse Scripts and include extended scripts');
        if(parseScripts) then begin
            parseScriptsCb.State := cbChecked;
        end;

        blacklistScriptsCb       := CreateCheckbox(processingGroup, 10, 115,'Use Blacklist');

        if (resourceBlackList <> nil) then begin
            if(useResourceBlacklist) then begin
                blacklistScriptsCb.State := cbChecked;
            end;
        end else begin
            blacklistScriptsCb.Enabled := false;
        end;

        addAnimFolderCb := CreateCheckbox(processingGroup, 10, 135, 'Include Meshes\AnimTextData, if it exists');
        if(addAnimFolder) then begin
            addAnimFolderCb.State := cbChecked;
        end;

        addLodCb := CreateCheckbox(processingGroup, 10, 155, 'Include worldspace LOD data');
        if(addLod) then begin
            addLodCb.State := cbChecked;
        end;

        processNifsCb := CreateCheckbox(processingGroup, 10, 175, 'Parse NIFs for materials/textures');
        if(processNifs) then begin
            processNifsCb.State := cbChecked;
        end;
        //processNifs


        topOffset := topOffset + 200;

        outputGroup := CreateGroup(frm, 10, topOffset, 345, 90, 'Output');

        unpackExisting := CreateCheckbox(outputGroup, 10, 15,'Merge output with existing BA2');
        if(extractExistingBa2) then begin
            unpackExisting.state := cbChecked;
        end;

        repackOutput   := CreateCheckbox(outputGroup, 10, 35,'Repack output to new BA2');
        if(packNewBa2) then begin
            repackOutput.state := cbChecked;
        end;

        compressMainBa2 := CreateCheckbox(outputGroup, 10, 55, 'Compress main BA2, if possible');
        if(shouldCompressMainBa2) then begin
            compressMainBa2.state := cbChecked;
        end;

        deleteDirectory   := CreateCheckbox(outputGroup, 10, 75,'Delete Folder after repacking');
        if(deleteDirectoryAfterwards) then begin
            deleteDirectory.state := cbChecked;
        end;

        //cbIgnoreNamespaceless
        // cbIncludeSource

        resultCode := frm.ShowModal;

        if(resultCode <> mrYes) then begin
            Result := false;
            frm.free();
            exit;
        end;

        if(assigned(resourceBlackList)) then begin
            useResourceBlacklist := (blacklistScriptsCb.state = cbChecked);
        end else begin
            useResourceBlacklist := false;
        end;
        deleteDirectoryAfterwards := (deleteDirectory.state = cbChecked);
        parseScripts := (parseScriptsCb.state = cbChecked);

        addSource   := (cbIncludeSource.State = cbChecked);
        addFacemesh := (includeFaceMeshes.State = cbChecked);
        addScolMesh := (includeScol.State = cbChecked);
        addAnimFolder := (addAnimFolderCb.State = cbChecked);
        processNifs := (processNifsCb.State = cbChecked);


        verboseMode := (verboseModeCb.State = cbChecked);
        showListOfAssets := (showListOfAssetsCb.State = cbChecked);
        showMissingAssets := (showMissingAssetsCb.State = cbChecked);

        shouldCompressMainBa2 := (compressMainBa2.state = cbChecked);

        useXwm:= (cbAutoXwm.State = cbChecked);

        extractExistingBa2 := (unpackExisting.State = cbChecked);
        packNewBa2 := (repackOutput.State = cbChecked);

        saveConfig();
        frm.free();
        //Result := 1;
    end;
    ////////////////// GUI END ///////////////////


    //////////////// OUTPUT BEGIN /////////////////
    function DeleteDirectory(path: string; childrenOnly: boolean): boolean;
    var
        searchResult : TSearchRec;
        curFile: string;
    begin
        Result := true;

        if FindFirst(path+'\*', faAnyFile, searchResult) = 0 then begin
            repeat
                // ignore . and ..
                if(searchResult.Name <> '.') and (searchResult.Name <> '..') then begin
                    curFile := path+'\'+searchResult.Name;

                    if((searchResult.attr and faDirectory) = faDirectory) then begin
                        // AddMessage('Is Dir? yes');
                        if(not DeleteDirectory(curFile, false)) then begin
                            Result := false;
                            exit;
                        end;
                    end else begin
                        // file
                        if(not DeleteFile(curFile)) then begin
                            Result := false;
                            exit;
                        end;
                    end;
                end;
            until FindNext(searchResult) <> 0;

            // Must free up resources used by these successful finds
            FindClose(searchResult);
        end;

        // delete self
        if(not childrenOnly) then begin
            if(not RemoveDir(path)) then begin
                Result := false;
                exit;
            end;
        end;
    end;

    function ensureDirectory(path: string): boolean;
    begin
        Result := true;
        if(not DirectoryExists(path)) then begin
            if(not CreateDir (path)) then begin
                AddMessage('Error: failed to create directory '+path);
                Result := false;
                exit;
            end;
        end;
    end;

    function ensurePath(path: string): boolean;
    var
        i, start: Integer;
        curChar, curPart, curSubpath: string;
    begin
        Result := true;
        start := 1;
        curSubpath := '';
        for i:= 0 to length(path) do begin
            curChar := copy(path, i, 1);
            if(curChar = '\') then begin
                // separator!
                curPart := copy(path, start, i-start+1);

                curSubpath := curSubpath+curPart;

                if(not ensureDirectory(curSubpath)) then begin
                    Result := false;
                    exit;
                end;
                //AddMessage('CurPart: '+curPart);

                start := i+1;
            end;
        end;
    end;

    function stripSlash(path: string): string;
    begin
        Result := path;

        if(SameText(copy(path, length(path), 1), '\')) then begin
            Result := copy(path, 0, length(path)-1);
        end;
    end;

    function getDatestring(): string;
    begin
        Result := FormatDateTime('yyyy_mm_dd_hh_nn_ss', Now());
        //(Time: TDateTime; out Hour: Word; out Minute: Word; out Second: Word; out MilliSecond: Word)
    end;

    function packBa2(sourceDir: string; targetBa2Path: string; textureMode: boolean; uncompressed: boolean): boolean;
    var
        //dateString: string;
        params: string;
        resultCode: LongWord;
    begin
        Result := false;
        //dateString := getDatestring();
        if(FileExists(targetBa2Path)) then begin
            if(verboseMode) then begin
                AddMessage('Deleting existing '+targetBa2Path);
            end;
            if(not DeleteFile(targetBa2Path)) then begin
                AddMessage('Failed deleting!');
                exit;
            end;
        end;

        sourceDir := stripSlash(sourceDir);

        params := '"'+sourceDir+'" -c="'+targetBa2Path+'" -r="'+sourceDir+'"';
        if(textureMode) then begin
            params := params + ' -f=DDS';
        end else begin
            if(uncompressed) then begin
                AddMessage('Creating uncompressed archive');
                params := params + ' -compression=None';
            end;
        end;

        if(verboseMode) then begin
            AddMessage('CLI: '+archive2path+' '+params);
        end;

        resultCode := ShellExecuteWait(
            TForm(frmMain).Handle,                  // parent window handle, use 0 for none
            'open',                                 // verb
            archive2path,                            // application
            params,                           // parameters
            '',                                     // working directory
            SW_HIDE
        );

        if(resultCode <> 0) then begin
            AddMessage('Packing '+targetBa2Path+' failed!');
        end;
        Result := (resultCode = 0);

    end;

    function unpackBa2(ba2path: string; targetDir: string): boolean;
    var
        //cli: string;
        params: string;
        resultCode: LongWord;
    begin
        //cli := archive2path+' archive2path';
        Result := true;
        targetDir := stripSlash(targetDir);
        params := '"'+ba2path+'" -e="'+targetDir+'"';

        //AddMessage('CLI: '+archive2path+' '+params);


        resultCode := ShellExecuteWait(
            TForm(frmMain).Handle,                  // parent window handle, use 0 for none
            'open',                                 // verb
            archive2path,                            // application
            params,                           // parameters
            '',                                     // working directory
            SW_HIDE                           // 0= SW_SHOWNORMAL = window mode; 4 = SW_SHOWNOACTIVATE
        );

        if(resultCode <> 0) then begin
            AddMessage('Unpacking '+ba2path+' failed!');
        end;
        Result := (resultCode = 0);
    end;


    function beginOutput(): boolean;
    var
        basePath, mainPath, texPath: string;
        mainba2, texba2, basename: string;
    begin
        Result := false;

        basePath := outputDir+currentFilename+'\';
        mainPath := basePath+'main\';
        texPath := basePath+'textures\';

        if(not ensurePath(mainPath)) then begin
            exit;
        end;

        if(not ensurePath(texPath)) then begin
            exit;
        end;

        // potentially extract existing ba2?
        if(extractExistingBa2) then begin
            basename := ExtractFileBasename(currentFilename);
            mainba2 := basename + ' - Main.ba2';
            texba2 := basename + ' - Textures.ba2';

            if(FileExists(DataPath+mainba2)) then begin
                AddMessage('Unpacking '+mainba2);
                if(not unpackBa2(DataPath+mainba2, mainPath)) then begin
                    Result := false;
                    exit;
                end;
            end;

            if(FileExists(DataPath+texba2)) then begin
                AddMessage('Unpacking '+texba2);
                if(not unpackBa2(DataPath+texba2, texPath)) then begin
                    Result := false;
                    exit;
                end;
            end;

        end;

        Result := true;
    end;

    function shouldBeUncompressed(basePath: string): boolean;
    var
        searchResult : TSearchRec;
        curFile, curExt, prevPath: string;
    begin
        if (not shouldCompressMainBa2) then begin
            Result := true;
            exit;
        end;

        Result := false;
        if (FindFirst(basePath+'\*', faAnyFile, searchResult) = 0) then begin
            repeat
                // ignore . and ..
                if(searchResult.Name <> '.') and (searchResult.Name <> '..') then begin
                    curFile := basePath+'\'+searchResult.Name;
                    if((searchResult.attr and faDirectory) = faDirectory) then begin
                        if(shouldBeUncompressed(curFile)) then begin
                            Result := true;
                            break;
                        end;
                    end else begin
                        // file
                        // ExtractFileExt seems to be destructive, or something
                        curExt := ExtractFileExt(searchResult.Name);
                        if ((SameText(curExt, '.wav')) or (SameText(curExt, '.xwm'))) then begin
                            AddMessage('Found a sound file, setting compression to NONE');
                            Result := true;
                            break;
                        end;

                        {
                        curExt := ExtractFileExt(searchResult.Name);

                        if (SameText(curExt, '.nif')) then begin
                            if (strEndsWith(LowerCase(basePath), 'precombined')) then begin
                                AddMessage('Found a precombined mesh, setting compression to NONE');
                                Result := true;
                                break;
                            end;
                        end;
                        }
                    end;
                end;
            until (FindNext(searchResult) <> 0);

            // Must free up resources used by these successful finds
            FindClose(searchResult);
        end;
    end;

    function finishOutput(): boolean;
    var
        basePath, mainPath, texPath: string;
        mainba2, texba2, basename: string;
    begin
        Result := true;

        if(not packNewBa2) then begin
            exit;
        end;
        Result := false;

        basePath := outputDir+currentFilename+'\';
        mainPath := basePath+'main\';
        texPath  := basePath+'textures\';

        basename := outputDir+ExtractFileBasename(currentFilename);

        if(hasMainOutput) then begin
            AddMessage('Packing main Ba2');
            if(not packBa2(mainPath, basename + ' - Main.ba2', false, shouldBeUncompressed(mainPath))) then begin
                exit;
            end;

        end;

        if(hasTextureOutput) then begin
            AddMessage('Packing texture Ba2');
            if(not packBa2(texPath, basename + ' - Textures.ba2', true, false)) then begin
                exit;
            end;
        end;

        if(deleteDirectoryAfterwards) then begin
            AddMessage('Deleting '+basePath);
            if(not DeleteDirectory(basePath, false)) then begin
                AddMessage('Deleting failed! Maybe clean it up manually');
            end;
        end;

        Result := true;

    end;

    function ShellCopy(src, dst: string): boolean;
    begin
        // mostly stolen from mteFunctions... Main change is the usage of ShellExecuteWait (to make it synchronous) and checking existence of dst to return whenever it worked
        ShellExecuteWait(TForm(frmMain).Handle, 'open', 'cmd', '/C copy /Y "'+src+'" "'+dst+'"', ExtractFilePath(src), SW_HIDE);
        Result := FileExists(dst);
    end;

    function addOutputFile(fName: string): boolean;
    var
        curExt : string;
        src, dst: string;
    begin
        Result := true;

        src := DataPath+fName;

        if(not FileExists(src)) then begin
            AddMessage('Something bad happened: source file doesn''t exists');
            Result := false;
            exit;
        end;

        if(SameText(ExtractFileExt(fName), '.dds') or SameText(ExtractFileExt(fName), '.tga')) then begin
            dst := outputDir+currentFilename+'\textures\'+fName;
            hasTextureOutput := true;
        end else begin
            dst := outputDir+currentFilename+'\main\'+fName;
            hasMainOutput := true;
        end;

        //maybe ResourceCopy
        ensurePath(ExtractFilePath(dst));
        if(verboseMode) then begin
            AddMessage(src+' -> '+dst);
        end;

        if(FileExists(dst)) then begin
            if(not DeleteFile(dst)) then begin
                AddMessage('Failed to overwrite '+dst);
                Result := false;
            end;
        end;

        if(not ShellCopy(src, dst)) then begin
            AddMessage('Failed to copy '+src+' to '+dst);
            Result := false;
        end;
    end;

    ///////////////// OUTPUT END //////////////////


    procedure FillExistingResourceList();
    var
        containers: TStringList;
        containerContent: TStringList;
        i, j: integer;
        curContainer: string;
    begin
        if(verboseMode) then begin
            AddMessage('Building list of existing assets');
        end;
        containers := TStringList.create;
        ResourceContainerList(containers);
        for i:=0 to containers.count-1 do begin
            curContainer := containers[i];
            ResourceList(curContainer, existingResourceList);
            {
            containerContent := TStringList.create;
            ResourceList(curContainer, containerContent);
            for j:=0 to containerContent.count-1 do begin
                existingResourceList.add(containerContent[j]);
            end;
            containerContent.free();
            }
        end;
        containers.free();
        if(verboseMode) then begin
            AddMessage('List of existing assets built.');
        end;
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    var
        blacklistPath: string;
    begin
        Result := 0;

        outputDir := ProgramPath+'asset-collector-output\';
        //outputDir := DataPath+'mod-assets\';
        currentFilename := '';

        archive2path := DataPath + '..\Tools\Archive2\Archive2.exe';

        blacklistPath := ScriptsPath + 'Collect Assets Blacklist.txt';
        if(fileExists(blacklistPath)) then begin
            resourceBlackList := TStringList.create;
            resourceBlackList.loadFromFile(blacklistPath);
        end else begin
            resourceBlackList := nil;
            AddMessage('Blacklist file not found. Blacklisting will not be available');
        end;

        if(not showConfigGui()) then begin
            Result := 1;
        end;

        if(showMissingAssets) then begin
            missingResourceList := TStringList.create;
            missingResourceList.CaseSensitive := false;
            missingResourceList.Duplicates := dupIgnore;

            existingResourceList := TStringList.create;
            existingResourceList.CaseSensitive := false;
            existingResourceList.Duplicates := dupIgnore;
            FillExistingResourceList();
            // debugDumpList();
        end else begin
            existingResourceList := nil;
            missingResourceList := nil;
        end;

        // prepare dirs
        if(not ensurePath(outputDir)) then begin
            AddMessage('Error: failed to create directory '+outputDir);
            Result := 1;
            exit;
        end;

        hasTextureOutput := false;
        hasMainOutput := false;
        AddMessage('Begin processing');
    end;

    procedure processQuest(e: IInterface);
    var
        i, j, k, l, numResponses: integer;
        curDial, curInfo, questGroup, dialGroup: IInterface;
        curSig, curPart, curFullPath, curFile, curVoiceFile, snam: string;
        searchResult : TSearchRec;
        curFormId: cardinal;
    begin
        // flash
        snam := GetElementEditValues(e, 'SNAM');
        if(snam <> '') then begin
            // AddMessage('SNAM '+snam);
            processResource('Interface\'+snam);
        end;

        //Result := nil;
        questGroup := ChildGroup(e);
        for i:=0 to ElementCount(questGroup)-1 do begin
            curDial := ElementByIndex(questGroup, i);
            curSig := Signature(curDial);
            if(curSig <> 'DIAL') then continue;

            dialGroup := ChildGroup(curDial);
            for j:=0 to ElementCount(dialGroup)-1 do begin
                curInfo := ElementByIndex(dialGroup, j);
                if (Signature(curInfo) <> 'INFO') then begin
                    continue;
                end;

                numResponses := ElementCount(ElementByPath(curInfo, 'Responses'));
                curPart := 'Sound\Voice\'+GetFileName(GetFile(e))+'\'; //+ voiceNames[i] + '\' + IntToHex(myFormId, 8) + '_1.fuz';

                curFullPath := DataPath + curPart;
                for k:=1 to numResponses do begin
                    //
                    //targetName := 'Sound\Voice\'+GetFileName(GetFile(e))+'\' + voiceNames[i] + '\' + IntToHex(myFormId, 8) + '_1.fuz';

                    if(not DirectoryExists(curFullPath)) then begin
                        exit;
                    end;

                    if FindFirst(curFullPath+'\*', faAnyFile, searchResult) = 0 then begin
                        repeat
                            // ignore . and ..
                            if(searchResult.Name <> '.') and (searchResult.Name <> '..') then begin
                                curFile := searchResult.Name;
                                //AddMessage('BLA '+curFile);
                                for l:=1 to numResponses do begin
                                    curFormId := FormId(curInfo) and $00FFFFFF;
                                    curVoiceFile := curPart+curFile+'\'+IntToHex(curFormId, 8)+'_'+IntToStr(l)+'.fuz';
                                    // AddMessage('BLA '+curVoiceFile);
                                    processSound(curVoiceFile);
                                end;

                            end;
                        until FindNext(searchResult) <> 0;

                        // Must free up resources used by these successful finds
                        FindClose(searchResult);
                    end;// endfind
                end;
            end;
        end;
    end;

    procedure processEffectShader(e: IInterface);
    var
        icon: string;
    begin
        icon := GetElementEditValues(e, 'ICON');
        if(icon = '') then exit;

        processTexture(icon);
    end;

    procedure processTextureSet(e: IInterface);
    var
        textures, curTex: IInterface;
        i: integer;
        curTextureName: string;
    begin
        textures := ElementByPath(e, 'Textures (RGB/A)');
        if(not assigned(textures)) then begin
            exit;
        end;

        for i := 0 to ElementCount(textures)-1 do begin
            curTex := ElementByIndex(textures, i);

            curTextureName := GetEditValue(curTex);

            if(curTextureName <> '') then begin
                processTexture(curTextureName);
            end;
        end;
    end;

    procedure processLight(e: IInterface);
    var
        gobo: IInterface;
        goboTex: string;
    begin
        gobo := ElementByPath(e, 'NAM0');
        goboTex := GetEditValue(gobo);

        processTexture(goboTex);

        // DefaultLightWhite01SpotGRDim
    end;

    procedure processLensFlare(e: IInterface);
    var
        sprites, curFlare: IInterface;
        i: integer;
        curTex: string;
    begin
        sprites := ElementByPath(e, 'Lens Flare Sprites');

        for i:=0 to ElementCount(sprites)-1 do begin
            curFlare := ElementByIndex(sprites, i);
            curTex := GetElementEditValues(curFlare, 'FNAM');
            processTexture(curTex);

        end;
    end;

    procedure processTerminal(e: IInterface);
    var
        menuItems, item: IInterface;
        i: integer;
        textureName: string;
    begin
        menuItems := ElementByPath(e, 'Menu Items');
        if(not assigned(menuItems)) then exit;
        for i:=0 to ElementCount(menuItems)-1 do begin
            item := ElementByIndex(menuItems, i);
            textureName := GetElementEditValues(item, 'VNAM');
            if(textureName <> '') then begin
                processTexture(textureName);
            end;
        end;
    end;

    procedure processWorld(e: IInterface);
    var
        icon, xwem: IInterface;
        edid: string;
    begin
        // addLod
        icon := GetElementEditValues(e, 'ICON');
        if(icon <> '') then begin
            processTexture(icon);
        end;
        xwem := GetElementEditValues(e, 'XWEM');

        if(addLod) then begin
            edid := EditorID(e);
            // check if we have LOD
            processResource('LODSettings\'+edid+'.LOD');
            processResourceDirectoryRecursive('Meshes\terrain\'+edid);
            processResourceDirectoryRecursive('Textures\terrain\'+edid);
            // textures: all under Textures\Terrain\<edid>
            // meshes: all under Meshes\terrain\<edid>

        end;
    end;


    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        elemFn, elemSig: string;
    begin
        Result := 0;

        // comment this out if you don't want those messages
        // processing code goes here

        elemFn := GetFileName(GetFile(e));
        if(currentFilename = '') then begin
            currentFilename := elemFn;
        end else begin
            if(elemFn <> currentFilename) then begin
                AddMessage('Error: This script can''t process more than one plugin at a time.');
                Result := 1;
                exit;
            end;
        end;

        processScripts(e);
        processModels(e);
        processSounds(e);

        elemSig := Signature(e);

        if(addFacemesh and (elemSig = 'NPC_')) then begin
            // H4X
            processNpcModels(e);
            exit;
        end;

        if(elemSig = 'CELL') then begin
            processPrecombines(e);
            exit;
        end;

        if(elemSig = 'VTYP') then begin
            processVoiceType(e);
            exit;
        end;

        if(elemSig = 'RACE') then begin
            processRace(e);
            exit;
        end;

        if(elemSig = 'QUST') then begin
            processQuest(e);
            exit;
        end;

        if(elemSig = 'MSWP') then begin
            processMatSwap(e);
            exit;
        end;

        if(elemSig = 'EFSH') then begin
            processEffectShader(e);
            exit;
        end;

        if(elemSig = 'TXST') then begin
            processTextureSet(e);
            exit;
        end;

        if(elemSig = 'LIGH') then begin
            processLight(e);
            exit;
        end;

        if(elemSig = 'LENS') then begin
            processLensFlare(e);
            exit;
        end;

        if(elemSig = 'TERM') then begin
            processTerminal(e);
            exit;
        end;

        if(elemSig = 'WRLD') then begin
            processWorld(e);
            exit;
        end;
    end;

    function extractDataSubpath(path: string): string;
    var
        dataPathLength, pathLength: integer;
        pathPart: string;
    begin
        pathLength := length(path);
        dataPathLength := length(DataPath);


        if(pathLength <= dataPathLength) then begin
            exit;
        end;

        pathPart := LowerCase(copy(path, 0, dataPathLength));

        if(pathPart <> LowerCase(DataPath)) then begin
            exit;
        end;

        Result := copy(path, dataPathLength+1, pathLength);
    end;

    procedure addFileToList(frm: TForm; fileName: string);
    var
        clb: TCheckListBox;
        i, added: integer;
        fileNameLowerCase: string;
    begin
        fileNameLowerCase := LowerCase(fileName);

        clb := TCheckListBox(frm.FindComponent('CheckListBox1'));

        for i := 0 to clb.Items.count-1 do begin
            if(LowerCase(clb.Items[i]) = fileNameLowerCase) then begin
                AddMessage(fileName+' already in list');
                exit;
            end;
        end;

        added := clb.Items.add(fileName);
        clb.Checked[added] := true;
    end;

    procedure addDirectoryToList(frm: TForm; path: string);
    var
        searchResult : TSearchRec;
        curFile, realPath: string;
    begin
        realPath := DataPath + path;


        if FindFirst(realPath+'\*', faAnyFile, searchResult) = 0 then begin
            repeat
                // ignore . and ..
                if(searchResult.Name <> '.') and (searchResult.Name <> '..') then begin
                    curFile := path+'\'+searchResult.Name;
                    //AddMessage('what '+curFile);

                    if((searchResult.attr and faDirectory) = faDirectory) then begin
                      //  AddMessage('Is Dir? yes');
                        addDirectoryToList(frm, curFile);
                    end else begin
                        // file
                        addFileToList(frm, curFile);
                    end;
                end;
            until FindNext(searchResult) <> 0;

            // Must free up resources used by these successful finds
            FindClose(searchResult);
        end;

    end;

    procedure addFileHandler(sender: TObject);
    var
        filePath: string;
        objFile: TOpenDialog;
    begin
        objFile := CreateOpenFileDialog('Select file to add', '', DataPath, true);
        try
            if objFile.Execute then begin
                filePath := objFile.FileName;
            end;
        finally
            objFile.free;
        end;

        if(filePath = '') then exit;

        filePath := extractDataSubpath(filePath);

        if(filePath = '') then begin
            AddMessage('Can only add files from the Data directory.');
            exit;
        end;

        addFileToList(sender.parent, filePath);

    end;

    procedure addDirHandler(sender: TObject);
    var
        dir: string;
        objFile: TOpenDialog;
        slashPos: integer;
    begin
        objFile := TOpenDialog.Create(nil);

        objFile.Title := 'Select any file in the directory to add';
        objFile.Options := objFile.Options;
        objFile.InitialDir  := DataPath;

        objFile.Filter := 'This directory only|*|Entire Data subdirectory|*';
        objFile.FilterIndex := 1;

        try
            if objFile.Execute then begin
                dir := objFile.FileName;
            end;
        finally
            objFile.free;
        end;

        if(dir = '') then exit;

        dir := extractDataSubpath(ExtractFilePath(dir));
        if(dir = '') then begin
            AddMessage('Can only add files from the Data directory.');
            exit;
        end;

        if(objFile.FilterIndex = 2) then begin
            // until the first \
            slashPos := pos('\', dir);
            dir := copy(dir, 0, slashPos);
        end;

        addDirectoryToList(sender.parent, stripSlash(dir));
    end;

    procedure resourceListResize(sender: TForm);
    var
        addFileBtn, addDirBtn: TButton;
    begin
        addFileBtn := sender.FindComponent('addFileBtn');
        addDirBtn := sender.FindComponent('addDirBtn');
        addFileBtn.Top := sender.Height-75;
        addDirBtn.Top := sender.Height-75;
    end;

    procedure cleanUp();
    begin
        if(existingResourceList <> nil) then begin
            existingResourceList.free();
            existingResourceList := nil;
        end;
        if(missingResourceList <> nil) then begin
            missingResourceList.free();
            missingResourceList := nil;
        end;
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        i, added: integer;
        frm: TForm;
        clb: TCheckListBox;
        addFileBtn: TButton;
        addDirBtn: TButton;
    begin
        if(missingResourceList <> nil) then begin
            if(missingResourceList.count > 0) then begin
                AddMessage('=== FOUND MISSING ASSETS ===');
                for i:=0 to missingResourceList.count-1 do begin
                    AddMessage(missingResourceList[i]);
                end;
                AddMessage('============================');
            end;
        end;
        if(currentFilename = '') then begin
            AddMessage('Nothing to do!');
            Result := 0;
            cleanUp();
            exit;
        end;

        if(addAnimFolder) then begin
            processResourceDirectoryRecursive('meshes\AnimTextData');
        end;

        if(resourceNames.count <= 0) then begin
            AddMessage('Found no resources. Nothing to do.');
            Result := 0;
            cleanUp();
            exit;
        end;

        resourceNames.sort();

        if(showListOfAssets) then begin
            // prepare list
            frm := frmFileSelect;
            frm.Width := 800;
            frm.Height := 500;
            addFileBtn := CreateButton(frm, 10, 425, 'Add File');
            addFileBtn.Name := 'addFileBtn';
            addFileBtn.onclick := addFileHandler;

            addDirBtn := CreateButton(frm, 100, 425, 'Add Directory');
            addDirBtn.Name := 'addDirBtn';
            addDirBtn.onclick := addDirHandler;

            frm.onresize := resourceListResize;
            try
                frm.Caption := 'Select resources to process';
                clb := TCheckListBox(frm.FindComponent('CheckListBox1'));
                //clb.Items.Add('<new file>');
                for i := 0 to resourceNames.count-1 do begin
                    added := clb.Items.add(resourceNames[i]);
                    clb.Checked[added] := true;
                end;


                if frm.ShowModal <> mrOk then begin
                    Result := 0;
                    cleanUp();
                    Exit;
                end;

                if(not beginOutput()) then begin
                    Result := 1;
                    cleanUp();
                    exit;
                end;

                for i := 0 to Pred(clb.Items.Count) do begin
                    if clb.Checked[i] then begin
                        if(not addOutputFile(clb.Items[i])) then begin
                            Result := 1;
                            AddMessage('ERROR: could not add file to output. This is probably some windows FS bullshit');
                            cleanUp();
                            exit;
                        end;
                    end;
                end;
            finally
                frm.Free;
            end;
        end else begin
            if(not beginOutput()) then begin
                Result := 1;
                cleanUp();
                exit;
            end;
            for i := 0 to resourceNames.count-1 do begin
                if(not addOutputFile(resourceNames[i])) then begin
                    Result := 1;
                    AddMessage('ERROR: could not add file to output. This is probably some windows FS bullshit');
                    cleanUp();
                    exit;
                end;
            end;
        end;

        if(not finishOutput()) then begin
            cleanUp();
            Result := 1;
            exit;
        end;

        AddMessage('========');
        AddMessage('FINISHED');
        AddMessage('========');
        AddMessage('Your files are under '+outputDir);

        cleanUp();

        Result := 0;
    end;

end.
