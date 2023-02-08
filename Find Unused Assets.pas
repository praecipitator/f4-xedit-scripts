{
    This should, in theory, scan the given file's BA2s, and report any files which are in there, but aren't used by the plugin itself.
    Run on a whole file.
}
unit FindUnusedAssets;
    uses PraUtil;//, PexParser;

    var
        archivesList: TStringList;
        unusedAssets: TStringList;
        notFoundAssets: TStringList;
        // these are ALL resources, to prevent double processing
        scriptNames: TStringList; // raw script names!

        nifNames: TStringList; // includes meshes\
        matNames: TStringList; // includes materials\
        texNames: TStringList; // includes textures\
        sndNames: TStringList; // includes sound\

        resourceNames: TStringList; // all resources which are used

        resourceBlackList: TStringList;

        // Config Stuff
        addSource: boolean;
        //deleteDirectoryAfterwards: boolean;
        addFacemesh: boolean;
        addScolMesh: boolean;

        // parseScripts: boolean;
        // useResourceBlacklist: boolean;
        addAnimFolder: boolean;




        currentFilename: string;
        outputDir: string;
        archive2path: string;

        hasTextureOutput: boolean;
        hasMainOutput: boolean;
        needsCleanup: boolean;
        mainba2, texba2: string;



    function ExtractFileBasename(filename: string): string;
    var
        curExt: string;
    begin
        curExt := ExtractFileExt(filename);

        Result := copy(filename, 0, length(filename)-length(curExt));
    end;

    function processResource(resName: string): boolean;
    begin
        if(resName = '') then begin
            Result := false;
            exit;
        end;
    
        // if ((not fileExists(DataPath+resName)) or (resourceNames.indexOf(resName) >= 0)) then begin
        if (resourceNames.indexOf(resName) >= 0) then begin
            Result := false;
            exit;
        end;

        {
        if (useResourceBlacklist) then begin
                if(resourceBlackList.indexOf(resName) >= 0) then begin
                    AddMessage('Resource blacklisted: '+resName);
                exit;
            end;
        end;
        }

        // AddMessage('Found resource '+resName);
        resourceNames.add(resName);

        // does this exist?
        if(not ResourceExists(resName)) then begin
            // AddMessage('Failed to find '+resName);
            notFoundAssets.add(resName);
        end;


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

    procedure processScriptBase(pexPath: string);
    var
        curName: string;
        i:integer;
    begin
        {
        if(not pexReadFile(pexPath)) then begin
            pexCleanUp();
            AddMessage('Failed to parse '+pexPath);
            exit;
        end;

        for i:=0 to pexExtendedObjects.count-1 do begin
            curName := pexExtendedObjects[i];
            if(curName <> '') then begin
//                AddMessage('Parent name: '+pexExtendedObjects[i]);
                processScriptName(pexExtendedObjects[i]);
            end;
        end;

        pexCleanUp();
        }
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
                {if(parseScripts) then begin
                    processScriptBase(pexPath);
                end;}
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

        if(texNames.indexOf(textureName) >= 0) then begin
            exit;
        end;

        texNames.add(textureName);
        // AddMessage('Found texture '+textureName);

        processResource(textureName);
    end;

    function loadTdfStructFromArchive(struct: TdfStruct; materialName: string): boolean;
    var
        i: integer;
        containers: TStringList;
        lastContainer: string;
    begin
        containers := TStringList.create;

        // AddMessage('Looking for '+materialName);
        ResourceCount(materialName, containers);
        for i:=0 to containers.count-1 do begin
            lastContainer := containers[i];
            // AddMessage(containers[i]);
        end;
        containers.free;

        if(lastContainer = '') then begin
            Result := false;
            exit;
        end;

        struct.LoadFromResource(containers, materialName);

        Result := true;
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
            processTexture(wbNormalizeResourceName(el[i].EditValue, resTexture));
        end;
    end;

    procedure processMaterial(materialName: string);
    var
        matFile: TdfStruct;
// TwbBGSMFile, TwbBGEMFile
        i: integer;
        el: TdfElement;
    begin
        if(materialName = '') then begin
            exit;
        end;

        if(matNames.indexOf(materialName) >= 0)  then begin
            exit;
        end;

        {
        if(not FileExists(DataPath+materialName)) then begin
            exit;
        end;
        }
        if SameText(ExtractFileExt(materialName), '.bgsm') then begin
            matFile := TwbBGSMFile.Create;
        end else if SameText(ExtractFileExt(materialName), '.bgem') then begin
            matFile := TwbBGEMFile.Create;
        end;

        // matFile.LoadFromResource(materialName);
        // matFile.LoadFromResource('SS2 - Main.ba2', materialName);
        // matFile.LoadFromResource('F:\SteamSSD\steamapps\common\Fallout 4\Data\Fallout4 - Materials.ba2', materialName);
        matNames.add(materialName);
        processResource(materialName);
        if(not loadTdfStructFromArchive(matFile, materialName)) then begin
            matFile.Free;
            // AddMessage('Failed to find '+materialName);
            exit;
        end;


        el := matFile.Elements['Textures'];
        if Assigned(el) then begin
            for i := 0 to Pred(el.Count) do begin
                processTexture(wbNormalizeResourceName(el[i].EditValue, resTexture));
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

    function loadNifFromArchive(struct: TwbNifFile; nifPath: string): boolean;
    var
        i: integer;
        containers: TStringList;
        lastContainer: string;
    begin
        containers := TStringList.create;

        // AddMessage('Looking for '+materialName);
        ResourceCount(nifPath, containers);
        for i:=0 to containers.count-1 do begin
            lastContainer := containers[i];
            // AddMessage(containers[i]);
        end;
        containers.free;

        if(lastContainer = '') then begin
            Result := false;
            exit;
        end;

        struct.LoadFromResource(containers, nifPath);

        Result := true;
    end;


    procedure processNif(nifPath: string);
    var
        i: integer;
        nif: TwbNifFile;
        Block: TwbNifBlock;
        curBlockName: string;
        hasMaterial: boolean;
    begin
        nif := TwbNifFile.Create;
        // mostly stolen from kinggath...
        if(not loadNifFromArchive(nif, nifPath)) then begin
            nif.free();
            exit;
        end;

        try
            //nif.LoadFromResource(nifPath);

            for i := 0 to Pred(Nif.BlocksCount) do begin
                Block := Nif.Blocks[i];
                // AddMessage('foo? '+Block.BlockType);
                if Block.BlockType = 'BSLightingShaderProperty' then begin
                    // check for material file in the Name field of FO4 meshes
                    hasMaterial := False;
                    if nif.NifVersion = nfFO4 then begin
                        // if shader material is used, get textures from it
                        // at this point, s is the material name
                        curBlockName := Block.EditValues['Name'];
                        if (SameText(ExtractFileExt(curBlockName), '.bgsm') or SameText(ExtractFileExt(curBlockName), '.bgem')) then begin
                            curBlockName := wbNormalizeResourceName(curBlockName, resMaterial);

                            processMaterial(curBlockName);
                            hasMaterial := True;
                        end;
                    end;
                    // no material used, get textures from texture set
                    if not hasMaterial then begin
                        GetTexturesFromTextureSet(Block.Elements['Texture Set'].LinksTo);
                    end;
                end else if(Block.BlockType = 'BSBehaviorGraphExtraData') then begin
                    curBlockName := Block.EditValues['Behavior Graph File'];
                    if(curBlockName <> '') then begin
                        //processResource
                        // ProcessResource('Meshes\'+curBlockName);
                        ProcessResource(wbNormalizeResourceName(curBlockName, resMesh));
                    end;
                end else if(Block.BlockType = 'BSEffectShaderProperty') then begin

                    processTexture(Block.EditValues['Source Texture']);
                    processTexture(Block.EditValues['Grayscale Texture']);
                    processTexture(Block.EditValues['Env Map Texture']);
                    processTexture(Block.EditValues['Normal Texture']);
                    processTexture(Block.EditValues['Env Mask Texture']);
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
    
    function normalizeSlashes(name: string): string;
    begin
        Result := StringReplace(name, '/', '\', [rfReplaceAll]);
    end;

    procedure processModel(modelName: string);
    var
        modelNameFull: string;
    begin
        //modelName := 'meshes\' + LowerCase(modelName);
        //processTexture();
        modelName := wbNormalizeResourceName(modelName, resMesh);

        if(nifNames.indexOf(modelName) >= 0) then begin
            // done that already
            exit;
        end;

        modelNameFull := DataPath+modelName;
        {
        if(not fileExists(modelNameFull)) then begin
            exit;
        end;
        }
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
            processMaterial(wbNormalizeResourceName(replacementMat, resMaterial));
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
    begin
        if(not assigned(e)) then begin
            exit;
        end;
        formIdHex := IntToHex(FixedFormID(e) and $00FFFFFF, 8);

        masterName := GetFileName(GetFile(MasterOrSelf(e)));

        previsFileName := 'Vis\'+masterName+'\'+formIdHex+'.uvd';
        // AddMessage('Vis filename: '+previsFileName);
        processResource(previsFileName);
    end;

    procedure processPhysicsMesh(e: IInterface);
    var
        meshName, formIdHex, masterName: string;
    begin

        masterName := GetFileName(GetFile(MasterOrSelf(e)));
        formIdHex := IntToHex(FixedFormID(e) and $00FFFFFF, 8);

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

    procedure processSound(sndName: string);
    var
        xwmName: string;
    begin
        if(sndNames.indexOf(sndName) >= 0) then begin
            exit;
        end;

        sndNames.add(sndName);
        if(SameText(ExtractFileExt(sndName), '.wav')) then begin
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

        soundList := ElementByName(e, 'Sounds');

        //dumpElement(soundList, '');
        if(assigned(soundList)) then begin
            for i := 0 to ElementCount(soundList)-1 do begin
                curSnd := ElementByIndex(soundList, i);

                curSndFilename := GetEditValue( ElementBySignature(curSnd, 'ANAM'));
// AddMessage('== curSndFilename 1: '+curSndFilename);
                // dumpElement(curSnd, '');
                processSound(wbNormalizeResourceName(curSndFilename, resSound));
            end;
        end;
{
        // it could also have ANAM right away
        soundList := ElementBySignature(e, 'ANAM');
        if(assigned(soundList)) then begin
            // soundList either linksTo, or is something else
            curSnd := LinksTo(soundList);
            if(assigned(soundList)) then begin
                AddMessage('== NOW ==');
                dumpElem(curSnd);
                if(Signature(curSnd) <> 'SOUN') then begin
                    exit;
                end;
            end;
AddMessage('== NOW2 ==');
            dumpElem(e);
            1bla
            curSndFilename := GetEditValue(soundList);


AddMessage('== curSndFilename 2: '+curSndFilename);
            processSound(wbNormalizeResourceName(curSndFilename, resSound));
        end;
}

    end;

    function stripSlash(path: string): string;
    begin
        Result := path;

        if(SameText(copy(path, length(path), 1), '\')) then begin
            Result := copy(path, 0, length(path)-1);
        end;
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
                processResource(wbNormalizeResourceName(curHkx, resMesh));
                //processResource('Meshes\'+curHkx);
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
        // nope for now
        exit;
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
                            processSound(wbNormalizeResourceName(basePath+'\'+searchResult.Name, resSound));
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

    {
    procedure processVoiceTypeNew(e: IInterface);
    var
        basePath: string;
    begin
        basePath := 'Sound\Voice\'+GetFileName(GetFile(e)) + '\' +EditorID(e);
        // what follows are editor ID (or that one override name) of relevant INFOs
        // how to find relevant INFOs?
        // - find all INFOs which reference the voicetype
        // - - find all QUSTs which reference the voicetype
        // - - - find corresponding aliases within the quest, if any

        // - find the relevant NPC(s)
        // - - find all INFOs which reference the NPC
        // - - find all QUSTs whcih reference the NPC
        // - - - find the corresponding aliases in the quest, if any

        // for relevant quest aliases:
        // - Iterate the quest's SCENs
        // - Iterate the SCEN's actions
        // - if ALID is the aliases ID, then... ooof
    end;
    }

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
        // parseScriptsCb: TCheckBox;
        blacklistScriptsCb: TCheckBox;
        compressMainBa2: TCheckBox;
        addAnimFolderCb: TCheckBox;

        resultCode: Integer;
    begin
        scriptNames := TStringList.create;
        nifNames := TStringList.create;
        matNames := TStringList.create;
        texNames := TStringList.create;
        sndNames := TStringList.create;
        resourceNames := TStringList.create;
        notFoundAssets := TStringList.create;

        needsCleanup := true;

        scriptNames.CaseSensitive := false;
        nifNames.CaseSensitive := false;
        matNames.CaseSensitive := false;
        texNames.CaseSensitive := false;
        sndNames.CaseSensitive := false;
        resourceNames.CaseSensitive := false;

        Result := true;

        windowHeightBase := 280;
        windowWidthBase := 360;
        topOffset := 0;

        frm := CreateDialog('Asset Collector', windowWidthBase, windowHeightBase+60);

        btnOkay := CreateButton(frm, 10, windowHeightBase, 'OK');
        btnOkay.ModalResult := mrYes;
        btnOkay.width := 75;

        btnCancel := CreateButton(frm, 90, windowHeightBase, 'Cancel');
        btnCancel.ModalResult := mrCancel;
        btnCancel.width := 75;

        topOffset := 10;
        processingGroup := CreateGroup(frm, 10, topOffset, 345, 150, 'Processing');

        cbIncludeSource         := CreateCheckbox(processingGroup, 10, 15,'Include Script Sources');
        cbIncludeSource.state := cbChecked;
        includeFaceMeshes       := CreateCheckbox(processingGroup, 10, 35,'Include Face Meshes');
        includeFaceMeshes.state := cbChecked;

        includeScol := CreateCheckbox(processingGroup, 10, 55, 'Include SCOL Meshes');

        //cbAutoXwm       := CreateCheckbox(processingGroup, 10, 75,'Use XWM sound files, if possible');
        //cbAutoXwm.State := cbChecked;

        //parseScriptsCb       := CreateCheckbox(processingGroup, 10, 95,'Parse Scripts and include extended scripts');
        // parseScriptsCb.State := cbChecked;

        // blacklistScriptsCb       := CreateCheckbox(processingGroup, 10, 115,'Use Blacklist');
{
        if (resourceBlackList <> nil) then begin
            blacklistScriptsCb.State := cbChecked;
        end else begin
            blacklistScriptsCb.Enabled := false;
        end;
}
        addAnimFolderCb := CreateCheckbox(processingGroup, 10, 135, 'Include Meshes\AnimTextData, if it exists');
        addAnimFolderCb.State := cbChecked;


        topOffset := topOffset + 160;



        //cbIgnoreNamespaceless
        // cbIncludeSource

        resultCode := frm.ShowModal;

        if(resultCode <> mrYes) then begin
            Result := false;
            exit;
        end;

        {
        if(assigned(resourceBlackList)) then begin
            useResourceBlacklist := (blacklistScriptsCb.state = cbChecked);
        end else begin
            useResourceBlacklist := false;
        end;
        }
        //deleteDirectoryAfterwards := (deleteDirectory.state = cbChecked);
        //parseScripts := (parseScriptsCb.state = cbChecked);

        addSource   := (cbIncludeSource.State = cbChecked);
        addFacemesh := (includeFaceMeshes.State = cbChecked);
        addScolMesh := (includeScol.State = cbChecked);
        addAnimFolder := (addAnimFolderCb.State = cbChecked);





        //Result := 1;
    end;
    ////////////////// GUI END ///////////////////


    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    var
        blacklistPath: string;
    begin
        Result := 0;
        needsCleanup := false;

        outputDir := ProgramPath+'asset-collector-output\';
        //outputDir := DataPath+'mod-assets\';
        currentFilename := '';

        archive2path := DataPath + '..\Tools\Archive2\Archive2.exe';

        blacklistPath := ProgramPath + 'Edit Scripts\Collect Assets Blacklist.txt';
        if(fileExists(blacklistPath)) then begin
            resourceBlackList := TStringList.create;
            resourceBlackList.loadFromFile(blacklistPath);
        end else begin
            resourceBlackList := nil;
            // AddMessage('Blacklist file not found. Blacklisting will not be available');
        end;

        if(not showConfigGui()) then begin
            Result := 1;
        end;

        hasTextureOutput := false;
        hasMainOutput := false;

        archivesList := TStringList.create;
        ResourceContainerList(archivesList);
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

        processTexture(wbNormalizeResourceName(icon, resTexture));
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
        end;

        {
        if(elemSig = 'CELL') then begin
            processPrecombines(e);
            exit;
        end;
        }

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
                // AddMessage(fileName+' already in list');
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

    function ExtractFileBasename(filename: string): string;
    var
        curExt: string;
    begin
        curExt := ExtractFileExt(filename);

        Result := copy(filename, 0, length(filename)-length(curExt));
    end;

    procedure stripNonModResourceNames();
    var
        i: integer;
        curContainers: TStringList;
    begin

        // AddMessage(mainba2);

        i:=0;
        while i<resourceNames.count do begin
            curContainers := TStringList.create;
            ResourceCount(resourceNames[i], curContainers);

            if(curContainers.indexOf(mainba2) < 0) then begin
                if(curContainers.indexOf(texba2) < 0) then begin
                    // remove
                    // AddMessage('Removing '+resourceNames[i]);
                    resourceNames.delete(i);
                    curContainers.free;
                    continue;
                end{ else begin
                    AddMessage('Found '+resourceNames[i]+' in '+texba2);
                end};
            end{ else begin
                AddMessage('Found '+resourceNames[i]+' in '+mainba2);
            end};

            curContainers.free;
            i := i+1;
        end;
    end;

    function isVoiceReallyUnused(voiceName: string): boolean;
    var
        regex: TPerlRegEx;
        masterFile, voiceName, formIdStr: string;
    begin
        //Sound\Voice\SS2.esm\SS2_VT_CarlMeade\0001DDB2_1.fuz
        regex := TPerlRegEx.Create();
        Result := true;
        try
            regex.RegEx := 'Sound\\Voice\\(.+)\\(.+)\\(.+)_[0-9]+\.fuz';
            regex.Subject := voiceName;

            if(regex.Match()) then begin
                // misnomer, is actually the highest valid index of regex.Groups
                if(regex.GroupCount >= 3) then begin
                    masterFile := regex.Groups[1];
                    voiceName := regex.Groups[2];
                    formIdStr  := regex.Groups[3];
                    AddMessage('Master: '+masterFile+', '+voiceFile+', '+formIdStr);
                    // Result := regex.Groups[returnMatchNr];
                end;
            end;
        finally
            RegEx.Free;
        end;
    end;


    procedure findUnusedAssetsInFile(ba2name: string);
    var
        ba2content: TStringList;
        i: integer;
        curFile: string;
    begin
        if(not fileExists(ba2name)) then exit;

        ba2content := TStringList.create;
        ba2content.CaseSensitive := false;
        ba2content.Duplicates := dupIgnore;

        ResourceList(ba2name, ba2content);

        ba2content.sort();

        for i:=0 to ba2content.count-1 do begin
            curFile := ba2content[i];

            if(strStartsWith(curFile, 'Meshes\AnimTextData\')) then begin
                continue;
            end;

            // skip voices, for now
            if(strEndsWith(curFile, '.fuz')) then begin
                if(isVoiceReallyUnused(curFile)) then begin
                    continue;
                end;
            end;

            if(resourceNames.indexOf(curFile) < 0) then begin
                //AddMessage(curFile+' seems to be unused');
                unusedAssets.add(curFile);
            end{ else begin
                AddMessage(curFile+' is used in '+ba2name);
            end};
        end;
            //if(unusedAssets

        ba2content.free();
    end;

    procedure cleanUp();
    begin
        if(not needsCleanup) then exit;
        resourceNames.free();
        scriptNames.free();
        nifNames.free();
        matNames.free();
        texNames.free();
        sndNames.free();
        notFoundAssets.free();
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
        basename, pathBase: string;
    begin
        basename := ExtractFileBasename(currentFilename);
        mainba2 := DataPath+basename + ' - Main.ba2';
        texba2 := DataPath+basename + ' - Textures.ba2';

        archivesList.free();

        if(currentFilename = '') then begin
            AddMessage('Nothing to do!');
            Result := 0;
            exit;
        end;

        if(addAnimFolder) then begin
            processResourceDirectoryRecursive('meshes\AnimTextData');
        end;

        if(resourceNames.count <= 0) then begin
            AddMessage('Found no resources. Nothing to do.');
            Result := 0;
            exit;
        end;

        resourceNames.sort();

        // now remove all resources which exist in other archives
        stripNonModResourceNames();

        unusedAssets := TStringList.create;
        // now go through both archives, and check if the files are in the resourceNames
        findUnusedAssetsInFile(mainba2);
        findUnusedAssetsInFile(texba2);

        notFoundAssets.sort();

        pathBase := ProgramPath+basename;

        if(unusedAssets.count > 0) then begin
            AddMessage('Saving unused assets to '+pathBase+' unused.txt');
            unusedAssets.saveToFile(pathBase+' unused.txt');
        end else begin
            AddMessage('No unused assets found');
        end;

        if(notFoundAssets.count > 0) then begin
            AddMessage('Saving missing assets to '+pathBase+' not found.txt');
            notFoundAssets.saveToFile(pathBase+' not found.txt');
        end else begin
            AddMessage('No missing assets found');
        end;


        unusedAssets.free();


        cleanUp();

        Result := 0;
    end;

end.
