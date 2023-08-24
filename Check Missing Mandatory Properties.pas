{
    Run on any amount of forms. 
    It will display missing mandatory properties.
    For forms, it will also attempt to auto-fill them, by looking up a form with the same name as the property.
    No type checking is done on the form before setting it yet, however.
    
    Also, alias auto-filling doesn't work yet, either.
    
    set autoFill to false below to disable autofilling. Maybe I'll make a GUI eventually.
}
unit CheckMissingMandatoryProps;
    uses PexToJson;
    uses praUtil;

    const
        progressMessageAfter = 1000;
        skipSomeSS2Forms = true;
        autoFill = true;

    var
        scriptCache, decompilerCache: TJsonObject;
        processedForms, decompiledScripts, foundErrors: cardinal;
        edidCache: TStringList;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        decompilerCache := TJsonObject.create();
        scriptCache := TJsonObject.create();
        AddMessage('Script begins.');
        processedForms := 0;
        edidCache := TStringList.create;
        edidCache.Duplicates := dupIgnore;
        edidCache.CaseSensitive := false;
        edidCache.Sorted := true;
        decompiledScripts := 0;
        foundErrors := 0;
    end;

    procedure addToJsonArray(newEntry: string; arr: TJsonArray);
    var
        i: integer;
    begin
        for i:=0 to arr.count-1 do begin
            if(LowerCase(arr.S[i]) = LowerCase(newEntry)) then exit;
        end;


        arr.add(newEntry);
    end;

    function GetFormByEdidCached(edid: string): IInterface;
    var
        i: integer;
    begin
        i := edidCache.indexOf(edid);
        if(i >= 0) then begin
            Result := ObjectToElement(edidCache.Objects[i]);
            exit;
        end;

        Result := GetFormByEdid(edid);
        edidCache.AddObject(edid, result);
    end;


    procedure fillScriptValue(propType: string; prop, rawPropVal: IInterface; propObj: TJsonObject);
    var
        curObj: IInterface;
    begin
        if (assigned(rawPropVal)) then begin
            // propObj.B['hasValue'] := true;
            if(propType = 'String') then begin
                propObj.S['value'] := geevt(prop, propType);
                exit;
            end;

            if(propType = 'Int32') then begin
                propObj.I['value'] := StrToInt(geevt(prop, propType));
                exit;
            end;

            if(propType = 'Float') then begin
                propObj.F['value'] := StrToFloat(geevt(prop, propType));
                exit;
            end;

            if(propType = 'Bool') then begin
                propObj.B['value'] := StrToBool(geevt(prop, propType));
                exit;
            end;

            // Object
            if(propType = 'Object') then begin
                curObj := pathLinksTo(prop, 'Value\Object Union\Object v2\FormID');
                if(assigned(curObj)) then begin
                    propObj.S['value'] := FormToAbsStr(curObj);
                end else begin
                    propObj.S['value'] := '';
                end;
                exit;
            end;

            // not doing the rest yet

        end;
    end;

    procedure getScriptProperties(e: IInterface; outList: TJsonObject);
    var
        curScript, scripts, propRoot, prop, rawPropVal, propObj: IInterface;
        i, j: integer;
        scriptName, scriptNameLc, propName, propType: string;
        scriptObj: TJsonObject;
        propArray: TJsonArray;
    begin
        scripts := ElementByPath(e, 'VMAD - Virtual Machine Adapter\Scripts');
        if(not assigned(scripts)) then exit;

        // processing code goes here
        for i := 0 to ElementCount(scripts)-1 do begin
            curScript := ElementByIndex(scripts, i);
            scriptName := geevt(curScript, 'scriptName');
            scriptNameLc := LowerCase(scriptName);

            propRoot := ElementByPath(curScript, 'Properties');

            scriptObj := outList.O[scriptNameLc];
            scriptObj.S['name'] := scriptName;

            if(assigned(propRoot)) then begin

                // propArray := scriptObj.A['properties'];


                for i := 0 to ElementCount(propRoot)-1 do begin
                    prop := ElementByIndex(propRoot, i);
                    propName := geevt(prop, 'propertyName');
                    propType := geevt(prop, 'Type');

                    propObj := scriptObj.O['properties'].O[LowerCase(propName)]; //outList.O['properties'].O[LowerCase(propName)];

                    propObj.S['name'] := propName;
                    propObj.S['type'] := propType;

                    rawPropVal := getRawScriptProp(curScript, propName);
                    fillScriptValue(propType, prop, rawPropVal, propObj);
                end;
            end;

        end;
    end;

    procedure getFilledProperties(e: IInterface; outList: TJsonObject);
    var
        curScript, scripts, propRoot, prop: IInterface;
        i, j: integer;
        scriptName, scriptNameLc, propName: string;
        scriptObj: TJsonObject;
        propArray: TJsonArray;
    begin
        scripts := ElementByPath(e, 'VMAD - Virtual Machine Adapter\Scripts');
        if(not assigned(scripts)) then exit;

        // processing code goes here
        for i := 0 to ElementCount(scripts)-1 do begin
            curScript := ElementByIndex(scripts, i);
            scriptName := geevt(curScript, 'scriptName');
            scriptNameLc := LowerCase(scriptName);

            propRoot := ElementByPath(curScript, 'Properties');

            scriptObj := outList.O[scriptNameLc];
            scriptObj.S['name'] := scriptName;

            if(assigned(propRoot)) then begin

                propArray := scriptObj.A['properties'];

                for i := 0 to ElementCount(propRoot)-1 do begin
                    prop := ElementByIndex(propRoot, i);
                    propName := geevt(prop, 'propertyName');

                    addToJsonArray(propName, propArray);
                end;
            end;

        end;
    end;

    function readPexScriptCached(scriptName: string): TJsonObject;
    var
        curPexData: IInterface;
        scriptKey, curScriptName: string;
        done: boolean;
    begin
        scriptKey := LowerCase(scriptName);
        
        if(decompilerCache.Types[scriptKey] = JSON_TYPE_OBJECT) then begin
            Result := decompilerCache.O[scriptKey];
            exit;
        end;
        if(decompilerCache.Types[scriptKey] = JSON_TYPE_BOOL) then begin
            Result := nil;
            exit;
        end;
        //JSON_TYPE_BOOL
        // AddMessage('Decompiling '+scriptName);

        Result := readPexScriptName(scriptName);
        if(Result = nil) then begin
            AddMessage('Failed to decompile '+scriptName);
            decompilerCache.B[scriptKey] := false;
        end else begin
            decompilerCache.O[scriptKey] := Result;
        end;
    end;

    function readPexScriptFull(scriptName: string; mergeWith: TJsonObject): TJsonObject;
    var
        curPexData, curObject: IInterface;
        curScriptName: string;
        done: boolean;
        i, j: integer;
        curObjectsArray: TJsonArray;
    begin
        // TODO potentially blacklist all vanilla scripts
        if
            (scriptName = '') or
            (scriptName = 'Action') or
            (scriptName = 'Activator') or
            (scriptName = 'ActiveMagicEffect') or
            (scriptName = 'Actor') or
            (scriptName = 'ActorBase') or
            (scriptName = 'ActorValue') or
            (scriptName = 'Alias') or
            (scriptName = 'Ammo') or
            (scriptName = 'Armor') or
            (scriptName = 'Book') or
            (scriptName = 'CameraShot') or
            (scriptName = 'Cell') or
            (scriptName = 'Class') or
            (scriptName = 'CombatStyle') or
            (scriptName = 'Component') or
            (scriptName = 'ConstructibleObject') or
            (scriptName = 'Container') or
            (scriptName = 'Door') or
            (scriptName = 'Form') or
            (scriptName = 'Enchantment') or
            (scriptName = 'EncounterZone') or
            (scriptName = 'Explosion') or
            (scriptName = 'Faction') or
            (scriptName = 'Flora') or
            (scriptName = 'FormList') or
            (scriptName = 'Furniture') or
            (scriptName = 'Game') or
            (scriptName = 'GlobalVariable') or
            (scriptName = 'Hazard') or
            (scriptName = 'HazardBase') or
            (scriptName = 'HeadPart') or
            (scriptName = 'Quest') or
            (scriptName = 'Holotape') or
            (scriptName = 'Idle') or
            (scriptName = 'IdleMarker') or
            (scriptName = 'ImageSpaceModifier') or
            (scriptName = 'ImpactDataSet') or
            (scriptName = 'Ingredient') or
            (scriptName = 'InputEnableLayer') or
            (scriptName = 'InstanceNamingRules') or
            (scriptName = 'Key') or
            (scriptName = 'Keyword') or
            (scriptName = 'LeveledActor') or
            (scriptName = 'LeveledItem') or
            (scriptName = 'LeveledSpell') or
            (scriptName = 'Light') or
            (scriptName = 'Location') or
            (scriptName = 'LocationAlias') or
            (scriptName = 'LocationRefType') or
            (scriptName = 'MagicEffect') or
            (scriptName = 'Math') or
            (scriptName = 'Message') or
            (scriptName = 'MiscObject') or
            (scriptName = 'MovableStatic') or
            (scriptName = 'MusicType') or
            (scriptName = 'ObjectMod') or
            (scriptName = 'ObjectReference') or
            (scriptName = 'Outfit') or
            (scriptName = 'OutputModel') or
            (scriptName = 'Package') or
            (scriptName = 'Perk') or
            (scriptName = 'Potion') or
            (scriptName = 'Projectile') or
            (scriptName = 'Race') or
            (scriptName = 'Scene') or
            (scriptName = 'ShaderParticleGeometry') or
            (scriptName = 'Shout') or
            (scriptName = 'SoulGem') or
            (scriptName = 'Sound') or
            (scriptName = 'SoundCategory') or
            (scriptName = 'SoundCategorySnapshot') or
            (scriptName = 'Spell') or
            (scriptName = 'Static') or
            (scriptName = 'TalkingActivator') or
            (scriptName = 'Terminal') or
            (scriptName = 'TextureSet') or
            (scriptName = 'Topic') or
            (scriptName = 'TopicInfo') or
            (scriptName = 'VisualEffect') or
            (scriptName = 'VoiceType') or
            (scriptName = 'Weapon') or
            (scriptName = 'Weather') or
            (scriptName = 'WordOfPower') or
            (scriptName = 'WorldSpace') or
            (scriptName = 'ScriptObject')
        then begin
            exit;
        end;

        if(mergeWith = nil) then begin
            Result := TJsonObject.create();
        end else begin
            Result := mergeWith;
        end;

        //AddMessage('Reading '+scriptName);
        //decompilerCache
        // Result := TJsonObject.create();
        curScriptName := scriptName;
        done := false;
        //while(not done) do begin
        curPexData := readPexScriptCached(scriptName);
        if(curPexData = nil) then begin
            done := true;
            exit;
        end;

        // Result.O['header'] := curPexData.O['header'];
        // Result.O['userFlags'] := curPexData.O['userFlags'];
        // merge "objects"
        for i:=0 to curPexData.A['objects'].count - 1 do begin
            curObject := curPexData.A['objects'].O[i];
            curObjectsArray := Result.A['objects'];
            //AddMessage('');
            appendObjectToArray(curObjectsArray, curObject);
            // Result.A['objects'].Add(curPexData.A['objects'].O[i]);

            readPexScriptFull(curObject.S['extends'], Result);
        end;

          //  exit; // DEBUG
        //end;
    end;

    function getMandatoryProperties(scriptName: string): TJsonObject;
        var
        pexData, curObj, curProp, propList, newPropEntry: TJsonObject;
        propRoot, objRoot: TJsonArray;
        i, j: integer;
        curPropName: string;
        userFlags: cardinal;
    begin
        if(scriptName = '') then exit;
        if(scriptCache.Types[scriptName] = JSON_TYPE_OBJECT) then begin
            Result := scriptCache.O[scriptName];
            exit;
        end;

        Result := scriptCache.O[scriptName];

        propList := Result.O['properties'];

        pexData := readPexScriptFull(scriptName, nil);
        decompiledScripts := decompiledScripts + 1;
        if(pexData = nil) then begin
            AddMessage('WARNING: Failed to decompile '+scriptName);
            exit;
        end;

        objRoot := pexData.A['objects'];

        for i:=0 to objRoot.count-1 do begin
            curObj := objRoot.O[i];
            if(LowerCase(curObj.S['name']) = LowerCase(scriptName)) then begin
                propRoot := curObj.A['properties'];
                // iterate props
                for j:=0 to propRoot.count-1 do begin
                    curProp := propRoot.O[j];
                    //AddMessage('checking prop '+curProp.toString());

                    curPropName := curProp.S['name'];
                    newPropEntry := propList.O[LowerCase(curPropName)];
                    userFlags := curProp.U['userFlags'];

                    newPropEntry.S['name'] := curPropName;
                    newPropEntry.B['hidden'] := ((userFlags and PEX_FLAG_HIDDEN) <> 0);
                    newPropEntry.B['mandatory'] := ((userFlags and PEX_FLAG_MANDATORY) <> 0);
                    newPropEntry.S['type'] := curProp.S['type'];
                end;
                break;
            end;
        end;

        //pexData.free();
    end;

    function shouldProcess(baseObj: IInterface): boolean;
    var
        edid, edidLowerCase: string;
    begin
        Result := false;
        if(not skipSomeSS2Forms) then begin
            Result := true;
            exit;
        end;

        edid := EditorID(baseObj);

        if(strStartsWith(edid, 'RECYCLED_MISC_')) then exit;

        edidLowerCase := LowerCase(edid);

        Result := (Pos('template', edidLowerCase) <= 0);
    end;

    function isPropTypeArray(propType: string): boolean;
    begin
        Result := strEndsWith(propType, '[]');
    end;

    function isPropTypeScalar(propType: string): boolean;
    var
        propTypeLc: string;
    begin
        propTypeLc := LowerCase(propType);
        Result := false;
        if
            (propTypeLc = 'int') or
            (propTypeLc = 'float') or
            (propTypeLc = 'string') or
            (propTypeLc = 'bool')
        then begin
            Result := true;
            exit;
        end;
    end;

    function isPropTypeAlias(propType: string): boolean;
    var
        propTypeLc: string;
    begin
        propTypeLc := LowerCase(propType);
        Result := false;
        if
            (propTypeLc = 'alias') or
            (propTypeLc = 'referencealias') or
            (propTypeLc = 'locationalias') or
            (propTypeLc = 'refcollectionalias')
        then begin
            Result := true;
            exit;
        end;
    end;

    function attemptAutofill(script: IInterface; propName, propType: string): boolean;
    var
        foundForm : IInterface;
    begin
        Result := false;
        if(not autoFill) then exit;

        foundForm := GetFormByEdidCached(propName);
        if(not assigned(foundForm)) then begin
            exit;
        end;
        // TODO: also check the signature and script of foundForm

        setScriptProp(script, propName, foundForm);
        Result := true;
    end;

    procedure processScriptLists(e: IInterface; scriptName: string; elementProps, pexProps: TJsonObject);
    var
        i: integer;
        curPropName, curType, curScriptName, curPropDisplayName: string;
        missingList, autoFillList: TJsonObject;
        script: IInterface;

        isScalar, isAlias, isAutofilled, isMandatory: boolean;

    begin
        script := getScript(e, scriptName);
        autoFillList := TJsonObject.create;
        missingList := TJsonObject.create;

        for i:=0 to pexProps.count-1 do begin
            curPropName := pexProps.Names[i];
            if(elementProps.Types[curPropName] <> JSON_TYPE_OBJECT) then begin
                // curPropDisplayName := pexProps[curPropn
                // prop is missing
                curType := pexProps.O[curPropName].S['type'];
                curPropDisplayName := pexProps.O[curPropName].S['name'];
                // AddMessage('Missing prop: '+curPropName);

                if(pexProps.O[curPropName].B['hidden']) then continue;

                if(isPropTypeArray(curType)) then continue;

                isScalar := isPropTypeScalar(curType);
                isAlias := isPropTypeAlias(curType);

                isMandatory := pexProps.O[curPropName].B['mandatory'];
                
                isAutofilled := false;

                {
                if(isAlias) then begin
                    // todo: figure out how to autofill aliases
                end;
                }


                if(not isAlias) and (not isScalar) then begin
                    // hopefully, this is a form
                    // AddMessage('I think type '+curType+' is a form');
                    if(attemptAutofill(script, curPropDisplayName, curType)) then begin
                        isAutofilled := true;
                        //AddMessage('Auto-filled '+scriptName+':'+curPropName);
                        autoFillList.A[scriptName].add(curPropDisplayName);
                    end;
                end;
                
                if(not isAutofilled and isMandatory) then begin
                    missingList.A[scriptName].add(curPropDisplayName);
                    foundErrors := foundErrors + 1;
                end;
            end;
        end;


        // AddMessage(missingList.toString());
        reportAutofill(e, autoFillList);
        reportError(e, missingList);
        missingList.free();
        autoFillList.free();
    end;
    
    procedure reportAutofill(e: IInterface; missingList: TJsonObject);
    var
        i, j: integer;
        scriptKey, scriptName, propName: string;
    begin
        if(missingList.count = 0) then exit;
        AddMessage('- Autofilled Properties -');
        AddMessage('Form: '+FullPath(e));
        for i:=0 to missingList.count-1 do begin
            scriptKey := missingList.Names[i];
            AddMessage('  Script: '+scriptKey);
            // AddMessage('    Properties: ');

            for j:= 0 to missingList.A[scriptKey].count-1 do begin
                AddMessage('     - '+missingList.A[scriptKey].S[j]);
            end;

        end;
        AddMessage('--------------------------');
    end;

    procedure reportError(e: IInterface; missingList: TJsonObject);
    var
        i, j: integer;
        scriptKey, scriptName, propName: string;
    begin
        if(missingList.count = 0) then exit;
        AddMessage('=== MISSING PROPERTIES ===');
        AddMessage('Form: '+FullPath(e));
        for i:=0 to missingList.count-1 do begin
            scriptKey := missingList.Names[i];
            AddMessage('  Script: '+scriptKey);
            //AddMessage('    Properties: ');

            for j:= 0 to missingList.A[scriptKey].count-1 do begin
                AddMessage('     - '+missingList.A[scriptKey].S[j]);
            end;

        end;
        AddMessage('==========================');
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        curScript, scripts, baseObj: IInterface;
        i, j: integer;
        curList: TJsonObject;
        curNameLC, curName, asd: string;
        curScriptList: TJsonObject;
        mandatoryList: TJsonObject;
        missingList: TStringList;
    begin
        Result := 0;

        // comment this out if you don't want those messages

        if(isReferenceSignature(Signature(e))) then begin
            curList := TJsonObject.create;
            baseObj := pathLinksTo(e, 'NAME');

            getScriptProperties(e, curList);
            getScriptProperties(baseObj, curList);
        end else begin
            if(not shouldProcess(e)) then exit;
            curList := TJsonObject.create;
            getScriptProperties(e, curList);
        end;

        // exit; // for now

        for i:=0 to curList.count-1 do begin
            curNameLC := curList.Names[i];
            curName := curList.O[curNameLC].S['name'];
            // AddMessage('checking #'+IntToStr(i)+' curname='+curName);

            curScriptList := curList.O[curNameLC];

            mandatoryList := getMandatoryProperties(curName);
            if(mandatoryList = nil) then continue;

            processScriptLists(e, curName, curScriptList.O['properties'], mandatoryList.O['properties']);



        end;

        processedForms := processedForms + 1;

        if((processedForms mod progressMessageAfter) = 0) then begin
            AddMessage('Checked '+IntToStr(processedForms)+' forms, decompiled '+IntToStr(decompiledScripts)+' scripts, found '+IntToStr(foundErrors)+' errors');
        end;



        curList.free();

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        AddMessage('Script done. Processed forms: '+IntToStr(processedForms)+'. Decompiled scripts: '+IntToStr(decompiledScripts)+'. Errors: '+IntToStr(foundErrors));
        Result := 0;
        scriptCache.free();
        decompilerCache.free();
        edidCache.free();
    end;

end.