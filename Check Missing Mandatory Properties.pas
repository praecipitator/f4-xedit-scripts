{
    Run on any amount of forms. A config UI will be shown before processing.
    Missing mandatory properties will be output to Messages.

    There is limited support for auto-filling. This will still generate output.
    It should be able to auto-fill properties, if the property type corresponds to a native form type, but won't work for scripts which extend such a type yet.
    For example, it should be able to autofill
        `MiscObject property BobbyPin auto const mandatory`
    because there is a misc item with the editor ID 'BobbyPin', but it will fail for
        `MyScript property MyExtendedMisc auto const mandatory`
    even if MyScript extends MiscObject, and there is a misc item with the editor ID 'MyExtendedMisc' with the script 'MyScript'.
    
    
    Explanation of the settings in the config UI:
        - Progress Message After: 
            After this number of forms have been checked, a message will be generated.
        - Auto-Fill Mandatory:
            Whenever auto-fill should be attempted, if a missing Mandatory property is found.
        - Auto-Fill Optional:
            Whenever auto-fill should be attempted, if a missing Optional property is found.
        - Auto-Fill Basic Types:
            If enabled, auto-fill will be attempted for properties of type ScriptObject and Form, too. 
            In this case, no type checking is done. If the Editor ID matches, it will be filled.
            This option is irrelevant if both Auto-Fill Mandatory and Auto-Fill Optional are disabled.
        - Skip some SS2 forms:
            SS2 contains some templates where mandatory properties aren't filled on purpose, as well
            as "recycled MISCs", which have a script, but no properties. 
            If they aren't skipped and the script is used on SS2, it will generate lots of false positives.
            However, `shouldProcess` might be a bit too loose, since it will skip everything with "template" in the EditorID.


}
unit CheckMissingMandatoryProps;
    uses PexToJson;
    uses praUtil;

    const

        SCRIPT_TYPE_INVALID = -1; // not anything known
        SCRIPT_TYPE_INTERNAL = 0; // stuff which shouldn't be extended or such
        SCRIPT_TYPE_BASE = 1;     // Form and ScriptObject
        SCRIPT_TYPE_RECORD = 2;   // base forms, like Actor or Keyword or Static
        SCRIPT_TYPE_REF = 3;      // any ObjectReference, REFR, ACHR, PGRE
        SCRIPT_TYPE_ALIAS = 4;    // any alias from a quest

        configFile = ProgramPath + 'Edit Scripts\Check Missing Mandatory Properties.cfg';

    var
        scriptCache, decompilerCache: TJsonObject;
        processedForms, decompiledScripts, foundErrors: cardinal;
        edidCache: TStringList;

        progressMessageAfter: integer;
        skipSomeSS2Forms: boolean;    // skips some templates, recycled miscs, etc
        autoFillMandatory: boolean;   // attempt to auto-fill mandatory properties
        autoFillOptional: boolean;    // attempt to auto-fill optional properties
        autoFillBasicTypes: boolean; // whenever to try to auto-fill properties of type Form or ScriptObject. No typechecking happens here, any form is fair game.

    procedure loadConfig();
    var
        i, j, breakPos: integer;
        curLine, curKey, curVal: string;
        lines : TStringList;
    begin
        // defaults
        progressMessageAfter := 1000;
        skipSomeSS2Forms := true;    // skips some templates, recycled miscs, etc
        autoFillMandatory := true;   // attempt to auto-fill mandatory properties
        autoFillOptional := true;    // attempt to auto-fill optional properties
        autoFillBasicTypes := false; // whenever to try to auto-fill properties of type Form or ScriptObject. No typechecking happens here, any form is fair game.

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

                if(curKey = 'progressMessageAfter') then begin
                    progressMessageAfter := StrToInt(curVal);
                end else if(curKey = 'skipSomeSS2Forms') then begin
                    skipSomeSS2Forms := StrToBool(curVal);
                end else if(curKey = 'autoFillMandatory') then begin
                    autoFillMandatory := StrToBool(curVal);
                end else if(curKey = 'autoFillOptional') then begin
                    autoFillOptional := StrToBool(curVal);
                end else if(curKey = 'autoFillBasicTypes') then begin
                    autoFillBasicTypes := StrToBool(curVal);
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

        lines.add('progressMessageAfter='+IntToStr(progressMessageAfter));
        lines.add('skipSomeSS2Forms='+BoolToStr(skipSomeSS2Forms));
        lines.add('autoFillMandatory='+BoolToStr(autoFillMandatory));
        lines.add('autoFillOptional='+BoolToStr(autoFillOptional));
        lines.add('autoFillBasicTypes='+BoolToStr(autoFillBasicTypes));

        lines.saveToFile(configFile);
        lines.free();
    end;

    function showGui(): boolean;
    var
        frm: TForm;
        eProgressAfter: TEdit;
        resultCode: cardinal;
        yOffset: integer;
        cbSkipForms, cbAutoFillMandatory, cbAutoFillOptional, cbAutoFillBasic: TCheckBox;

        btnOkay, btnCancel: TButton;
    begin
        frm := CreateDialog('Check Missing Mandatory Properties', 400, 200);

        eProgressAfter := CreateLabelledInput(frm, 20, 24, 100, 24, 'Progress Message After:', IntToStr(progressMessageAfter));

        //eProgressAfter.LabelPosition := lpLeft;
        // CreateLabel(frm, 10, 34, 'A progress message will be generated after this many forms were processed.');

        yOffset := 54;
        cbAutoFillMandatory := CreateCheckbox(frm, 20, yOffset, 'Auto-Fill Mandatory');
        cbAutoFillOptional := CreateCheckbox(frm, 210, yOffset, 'Auto-Fill Optional');

        yOffset := yOffset + 24;
        cbAutoFillBasic := CreateCheckbox(frm, 20, yOffset, 'Auto-Fill Basic Types');
        cbSkipForms := CreateCheckbox(frm, 210, yOffset, 'Skip some SS2 forms');

        cbSkipForms.checked := skipSomeSS2Forms;
        cbAutoFillMandatory.checked := autoFillMandatory;
        cbAutoFillOptional.checked := autoFillOptional;
        cbAutoFillBasic.checked := autoFillBasicTypes;
        yOffset := yOffset + 34;

        btnOkay := CreateButton(frm, 100, yOffset, 'OK');
        btnOkay.ModalResult := mrYes;
        btnOkay.width := 75;

        btnCancel := CreateButton(frm, 200, yOffset, 'Cancel');
        btnCancel.ModalResult := mrCancel;
        btnCancel.width := 75;

        resultCode := frm.ShowModal;
        if(resultCode <> mrYes) then begin
            Result := false;
            frm.free();
            exit;
        end;

        progressMessageAfter := StrToInt(trim(eProgressAfter.text));

        skipSomeSS2Forms := cbSkipForms.checked;
        autoFillMandatory := cbAutoFillMandatory.checked;
        autoFillOptional := cbAutoFillOptional.checked;
        autoFillBasicTypes := cbAutoFillBasic.checked;

        saveConfig();
        Result := true;
        frm.free();
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        loadConfig();
        if(not showGui()) then begin
            Result := 1;
            exit;
        end;

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

    procedure fillScriptProperties(curScript: IInterface; outList: TJsonObject);
    var
        scripts, propRoot, prop, rawPropVal, propObj: IInterface;
        i, j: integer;
        scriptName, scriptNameLc, propName, propType: string;
        scriptObj: TJsonObject;
        propArray: TJsonArray;
    begin
        scriptName := geevt(curScript, 'scriptName');
        scriptNameLc := LowerCase(scriptName);

        propRoot := ElementByPath(curScript, 'Properties');

        scriptObj := outList.O[scriptNameLc];
        scriptObj.S['name'] := scriptName;

        if(assigned(propRoot)) then begin

            for j := 0 to ElementCount(propRoot)-1 do begin
                prop := ElementByIndex(propRoot, j);
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

    procedure getScriptProperties(e: IInterface; outList: TJsonObject);
    var
        curScript, scripts, propRoot, prop, rawPropVal, propObj: IInterface;
        i, j: integer;
        scriptName, scriptNameLc, propName, propType: string;
        scriptObj: TJsonObject;
        propArray: TJsonArray;
    begin
        scripts := ElementByPath(e, 'VMAD - Virtual Machine Adapter\Scripts');
        if(assigned(scripts)) then begin

            // processing code goes here
            for i := 0 to ElementCount(scripts)-1 do begin
                curScript := ElementByIndex(scripts, i);
                fillScriptProperties(curScript, outList);
            end;
        end;

        curScript := getFragmentScript(e);
        if(assigned(curScript)) then begin
            fillScriptProperties(curScript, outList);
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

    {
        Returns types:

        SCRIPT_TYPE_INVALID = -1 // not anything known
        SCRIPT_TYPE_INTERNAL = 0 // stuff which shouldn't be extended or unused stuff
        SCRIPT_TYPE_BASE = 1 // Form and ScriptObject
        SCRIPT_TYPE_RECORD = 2 // base forms, like Actor or Keyword or Static
        SCRIPT_TYPE_REF = 3 // any ObjectReference, REFR, ACHR, PGRE(?)
        SCRIPT_TYPE_ALIAS = 4 // any alias from a quest
    }
    function getFormScriptType(scriptName: string): integer;
    var
        scriptNameLC: string;
    begin
        Result := SCRIPT_TYPE_INVALID;
        scriptNameLC := LowerCase(scriptName);

            // lets  put the unused ones here, too
        if
            (scriptNameLC = 'debug') or
            (scriptNameLC = 'f4se') or
            (scriptNameLC = 'game') or
            (scriptNameLC = 'input') or
            (scriptNameLC = 'inputenablelayer') or
            (scriptNameLC = 'instancedata') or
            (scriptNameLC = 'math') or
            (scriptNameLC = 'ui') or
            (scriptNameLC = 'utility') or
            (scriptNameLC = 'shout') or
            (scriptNameLC = 'wordofpower') or
            (scriptNameLC = 'leveledspell') or
            (scriptNameLC = 'scroll') or
            (scriptNameLC = 'soulgem')
        then begin
            Result := SCRIPT_TYPE_INTERNAL;
            exit;
        end;

        if
            (scriptNameLC = 'scriptobject') or
            (scriptNameLC = 'form')
        then begin
            Result := SCRIPT_TYPE_BASE;
            exit;
        end;


        if
            (scriptNameLC = 'objectreference') or
            (scriptNameLC = 'actor')
        then begin
            Result := SCRIPT_TYPE_REF;
            exit;
        end;


        if
            (scriptNameLC = 'alias') or
            (scriptNameLC = 'referencealias') or
            (scriptNameLC = 'locationalias') or
            (scriptNameLC = 'refcollectionalias')
        then begin
            Result := SCRIPT_TYPE_REF;
            exit;
        end;

        if
            (scriptNameLC = 'activemagiceffect') or
            (scriptNameLC = 'action') or
            (scriptNameLC = 'activator') or
            (scriptNameLC = 'flora') or
            (scriptNameLC = 'furniture') or
            (scriptNameLC = 'talkingactivator') or
            (scriptNameLC = 'actorbase') or
            (scriptNameLC = 'actorvalue') or
            (scriptNameLC = 'ammo') or
            (scriptNameLC = 'armor') or
            (scriptNameLC = 'associationtype') or
            (scriptNameLC = 'book') or
            (scriptNameLC = 'camerashot') or
            (scriptNameLC = 'cell') or
            (scriptNameLC = 'class') or
            (scriptNameLC = 'combatstyle') or
            (scriptNameLC = 'component') or
            (scriptNameLC = 'container') or
            (scriptNameLC = 'door') or
            (scriptNameLC = 'defaultobject') or
            (scriptNameLC = 'effectshader') or
            (scriptNameLC = 'enchantment') or
            (scriptNameLC = 'encounterzone') or
            (scriptNameLC = 'equipslot') or
            (scriptNameLC = 'explosion') or
            (scriptNameLC = 'faction') or
            (scriptNameLC = 'formlist') or
            (scriptNameLC = 'globalvariable') or
            (scriptNameLC = 'hazard') or
            (scriptNameLC = 'headpart') or
            (scriptNameLC = 'holotape') or
            (scriptNameLC = 'idle') or
            (scriptNameLC = 'idlemarker') or
            (scriptNameLC = 'imagespacemodifier') or
            (scriptNameLC = 'impactdataset') or
            (scriptNameLC = 'ingredient') or
            (scriptNameLC = 'instancenamingrules') or
            (scriptNameLC = 'keyword') or
            (scriptNameLC = 'locationreftype') or
            (scriptNameLC = 'leveledactor') or
            (scriptNameLC = 'leveleditem') or
            (scriptNameLC = 'leveledspell') or
            (scriptNameLC = 'light') or
            (scriptNameLC = 'location') or
            (scriptNameLC = 'magiceffect') or
            (scriptNameLC = 'message') or
            (scriptNameLC = 'miscobject') or
            (scriptNameLC = 'constructibleobject') or
            (scriptNameLC = 'key') or
            (scriptNameLC = 'soulgem') or
            (scriptNameLC = 'musictype') or
            (scriptNameLC = 'objectmod') or
            (scriptNameLC = 'outfit') or
            (scriptNameLC = 'outputmodel') or
            (scriptNameLC = 'package') or
            (scriptNameLC = 'perk') or
            (scriptNameLC = 'potion') or
            (scriptNameLC = 'projectile') or
            (scriptNameLC = 'quest') or
            (scriptNameLC = 'race') or
            (scriptNameLC = 'scene') or
            (scriptNameLC = 'scroll') or
            (scriptNameLC = 'shaderparticlegeometry') or
            (scriptNameLC = 'shout') or
            (scriptNameLC = 'sound') or
            (scriptNameLC = 'soundcategory') or
            (scriptNameLC = 'soundcategorysnapshot') or
            (scriptNameLC = 'spell') or
            (scriptNameLC = 'static') or
            (scriptNameLC = 'movablestatic') or
            (scriptNameLC = 'terminal') or
            (scriptNameLC = 'textureset') or
            (scriptNameLC = 'topic') or
            (scriptNameLC = 'topicinfo') or
            (scriptNameLC = 'visualeffect') or
            (scriptNameLC = 'voicetype') or
            (scriptNameLC = 'watertype') or
            (scriptNameLC = 'weapon') or
            (scriptNameLC = 'weather') or
            (scriptNameLC = 'wordofpower') or
            (scriptNameLC = 'worldspace') then
        begin
            Result := SCRIPT_TYPE_RECORD;
            exit;
        end;
    end;

    function scriptNameToSignature(scriptName: string): string;
    var
        scriptNameLC: string;
    begin
        Result := '';
        scriptNameLC := LowerCase(scriptName);

        if(scriptNameLC = 'objectreference') then begin
            Result := 'REFR';
            exit;
        end;
        if(scriptNameLC = 'actor') then begin
            Result := 'ACHR';
            exit;
        end;
        if(scriptNameLC = 'action') then begin
            Result := 'AACT';
            exit;
        end;
        if(scriptNameLC = 'activator') then begin
            Result := 'ACTI';
            exit;
        end;
        if(scriptNameLC = 'flora') then begin
            Result := 'FLOR';
            exit;
        end;
        if(scriptNameLC = 'furniture') then begin
            Result := 'FURN';
            exit;
        end;
        if(scriptNameLC = 'talkingactivator') then begin
            Result := 'TACT';
            exit;
        end;
        if(scriptNameLC = 'actorbase') then begin
            Result := 'NPC_';
            exit;
        end;
        if(scriptNameLC = 'actorvalue') then begin
            Result := 'AVIF';
            exit;
        end;
        if(scriptNameLC = 'ammo') then begin
            Result := 'AMMO';
            exit;
        end;
        if(scriptNameLC = 'armor') then begin
            Result := 'ARMO';
            exit;
        end;
        if(scriptNameLC = 'associationtype') then begin
            Result := 'ASTP';
            exit;
        end;
        if(scriptNameLC = 'book') then begin
            Result := 'BOOK';
            exit;
        end;
        if(scriptNameLC = 'camerashot') then begin
            Result := 'CAMS';
            exit;
        end;
        if(scriptNameLC = 'cell') then begin
            Result := 'CELL';
            exit;
        end;
        if(scriptNameLC = 'class') then begin
            Result := 'CLAS';
            exit;
        end;
        if(scriptNameLC = 'combatstyle') then begin
            Result := 'CSTY';
            exit;
        end;
        if(scriptNameLC = 'component') then begin
            Result := 'CMPO';
            exit;
        end;
        if(scriptNameLC = 'container') then begin
            Result := 'CONT';
            exit;
        end;
        if(scriptNameLC = 'door') then begin
            Result := 'DOOR';
            exit;
        end;
        if(scriptNameLC = 'defaultobject') then begin
            Result := 'DFOB';
            exit;
        end;
        if(scriptNameLC = 'effectshader') then begin
            Result := 'EFSH';
            exit;
        end;
        if(scriptNameLC = 'enchantment') then begin
            Result := 'ENCH';
            exit;
        end;
        if(scriptNameLC = 'encounterzone') then begin
            Result := 'ECZN';
            exit;
        end;
        if(scriptNameLC = 'equipslot') then begin
            Result := 'EQUP';
            exit;
        end;
        if(scriptNameLC = 'explosion') then begin
            Result := 'EXPL';
            exit;
        end;
        if(scriptNameLC = 'faction') then begin
            Result := 'FACT';
            exit;
        end;
        if(scriptNameLC = 'formlist') then begin
            Result := 'FLST';
            exit;
        end;
        if(scriptNameLC = 'globalvariable') then begin
            Result := 'GLOB';
            exit;
        end;
        if(scriptNameLC = 'hazard') then begin
            Result := 'HAZD';
            exit;
        end;
        if(scriptNameLC = 'headpart') then begin
            Result := 'HDPT';
            exit;
        end;
        if(scriptNameLC = 'holotape') then begin
            Result := 'NOTE';
            exit;
        end;
        if(scriptNameLC = 'idle') then begin
            Result := 'IDLE';
            exit;
        end;
        if(scriptNameLC = 'idlemarker') then begin
            Result := 'IDLM';
            exit;
        end;
        if(scriptNameLC = 'imagespacemodifier') then begin
            Result := 'IMAD';
            exit;
        end;
        if(scriptNameLC = 'impactdataset') then begin
            Result := 'IPDS';
            exit;
        end;
        if(scriptNameLC = 'ingredient') then begin
            Result := 'INGR';
            exit;
        end;
        if(scriptNameLC = 'instancenamingrules') then begin
            Result := 'INNR';
            exit;
        end;
        if(scriptNameLC = 'keyword') then begin
            Result := 'KYWD';
            exit;
        end;
        if(scriptNameLC = 'locationreftype') then begin
            Result := 'LCRT';
            exit;
        end;
        if(scriptNameLC = 'leveledactor') then begin
            Result := 'LVLN';
            exit;
        end;
        if(scriptNameLC = 'leveleditem') then begin
            Result := 'LVLI';
            exit;
        end;
        if(scriptNameLC = 'light') then begin
            Result := 'LIGH';
            exit;
        end;
        if(scriptNameLC = 'location') then begin
            Result := 'LCTN';
            exit;
        end;
        if(scriptNameLC = 'magiceffect') then begin
            Result := 'MGEF';
            exit;
        end;
        if(scriptNameLC = 'message') then begin
            Result := 'MESG';
            exit;
        end;
        if(scriptNameLC = 'miscobject') then begin
            Result := 'MISC';
            exit;
        end;
        if(scriptNameLC = 'constructibleobject') then begin
            Result := 'COBJ';
            exit;
        end;
        if(scriptNameLC = 'key') then begin
            Result := 'KEYM';
            exit;
        end;
        if(scriptNameLC = 'musictype') then begin
            Result := 'MUSC';
            exit;
        end;
        if(scriptNameLC = 'objectmod') then begin
            Result := 'OMOD';
            exit;
        end;
        if(scriptNameLC = 'outfit') then begin
            Result := 'OTFT';
            exit;
        end;
        if(scriptNameLC = 'outputmodel') then begin
            Result := 'SOPM';
            exit;
        end;
        if(scriptNameLC = 'package') then begin
            Result := 'PACK';
            exit;
        end;
        if(scriptNameLC = 'perk') then begin
            Result := 'PERK';
            exit;
        end;
        if(scriptNameLC = 'potion') then begin
            Result := 'ALCH';
            exit;
        end;
        if(scriptNameLC = 'projectile') then begin
            Result := 'PROJ';
            exit;
        end;
        if(scriptNameLC = 'quest') then begin
            Result := 'QUST';
            exit;
        end;
        if(scriptNameLC = 'race') then begin
            Result := 'RACE';
            exit;
        end;
        if(scriptNameLC = 'shaderparticlegeometry') then begin
            Result := 'SPGD';
            exit;
        end;
        if(scriptNameLC = 'sound') then begin
            Result := 'SNDR';
            exit;
        end;
        if(scriptNameLC = 'soundcategory') then begin
            Result := 'SNCT';
            exit;
        end;
        if(scriptNameLC = 'soundcategorysnapshot') then begin
            Result := 'SCSN';
            exit;
        end;
        if(scriptNameLC = 'spell') then begin
            Result := 'SPEL';
            exit;
        end;
        if(scriptNameLC = 'movablestatic') then begin
            Result := 'MSTT';
            exit;
        end;
        if(scriptNameLC = 'terminal') then begin
            Result := 'TERM';
            exit;
        end;
        if(scriptNameLC = 'textureset') then begin
            Result := 'TXST';
            exit;
        end;
        if(scriptNameLC = 'visualeffect') then begin
            Result := 'RFCT';
            exit;
        end;
        if(scriptNameLC = 'voicetype') then begin
            Result := 'VTYP';
            exit;
        end;
        if(scriptNameLC = 'watertype') then begin
            Result := 'WATR';
            exit;
        end;
        if(scriptNameLC = 'weapon') then begin
            Result := 'WEAP';
            exit;
        end;
        if(scriptNameLC = 'weather') then begin
            Result := 'WTHR';
            exit;
        end;
        if(scriptNameLC = 'worldspace') then begin
            Result := 'WLRD';
            exit;
        end;

        // for statics, just return STAT, do the SCOL handling outside
        if(scriptNameLC = 'static') then begin
            Result := 'STAT';
            exit;
        end;

        // within quest
        if(scriptNameLC = 'topic') then begin
            Result := 'DIAL';
            exit;
        end;
        if(scriptNameLC = 'topicinfo') then begin
            Result := 'INFO';
            exit;
        end;
        if(scriptNameLC = 'scene') then begin
            Result := 'SCEN';
            exit;
        end;

        // these aren't used and don't have signatures, it seems
        {
            shout
            wordofpower
            leveledspell
            scroll
            soulgem
        }
    end;

    function readPexScriptFull(scriptName: string; mergeWith: TJsonObject): TJsonObject;
    var
        curPexData, curObject: IInterface;
        curScriptName: string;
        done: boolean;
        i, j: integer;
        curObjectsArray: TJsonArray;
    begin
        if(scriptName = '') then exit;
        // TODO potentially blacklist all vanilla scripts
        if(getFormScriptType(scriptName) <> SCRIPT_TYPE_INVALID) then begin
            // "invalid" means: nothing known
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

    function getFormByTypeAndEdidCached(edid: string; typeString: string): IInterface;
    begin

    end;

    function attemptAutofill(script: IInterface; propName, propType: string; isMandatory: boolean): boolean;
    var
        foundForm : IInterface;
        formType: integer;
        sig: string;
    begin
        Result := false;
        if(isMandatory) then begin
            if(not autoFillMandatory) then exit;
        end else begin
            if(not autoFillOptional) then exit;
        end;

        formType := getFormScriptType(propType);
        case formType of
            SCRIPT_TYPE_INVALID, SCRIPT_TYPE_INTERNAL:
                begin
                    exit;
                end;
            SCRIPT_TYPE_BASE:
                begin
                    if(not autoFillBasicTypes) then begin
                        exit;
                    end;
                    foundForm := GetFormByEdidCached(propName);
                    Result := true;
                end;
            SCRIPT_TYPE_RECORD:
                begin
                    // okay...
                    sig := scriptNameToSignature(propType);
                    if(sig <> '') then begin
                        foundForm := FindObjectByEdidAndSignature(propName, sig);
                        // special handling
                        if(not assigned(foundForm)) then begin
                            if(sig = 'STAT') then begin
                                // also try SCOL
                                foundForm := FindObjectByEdidAndSignature(propName, 'SCOL');
                            end;
                        end;
                    end;
                end;
            SCRIPT_TYPE_REF:
                begin
                    foundForm := FindReferenceByEdid(propName);
                    // AddMessage('Auto-filling references is not yet implemented.');
                    // exit;
                end;
            SCRIPT_TYPE_ALIAS:
                begin
                    AddMessage('Auto-filling aliases is not yet implemented.');
                    exit;
                end;
        end;
        {SCRIPT_TYPE_INVALID = -1; // not anything known
        SCRIPT_TYPE_INTERNAL = 0; // stuff which shouldn't be extended or such
        SCRIPT_TYPE_BASE = 1;     // Form and ScriptObject
        SCRIPT_TYPE_RECORD = 2;   // base forms, like Actor or Keyword or Static
        SCRIPT_TYPE_REF = 3;      // any ObjectReference, REFR, ACHR, PGRE
        SCRIPT_TYPE_ALIAS = 4;    // any alias from a quest}


        // autoFillBasicTypes

        // AddMessage('Attempted AutoFill: '+propName+' of type '+propType);
        // foundForm := GetFormByEdidCached(propName);
        if(not assigned(foundForm)) then begin
            exit;
        end;

        setScriptProp(script, propName, foundForm);
        Result := true;
    end;

    function getScriptOrFragment(e: IInterface; scriptName: string): IInterface;
    begin
        Result := getFragmentScript(e);
        if (LowerCase(geevt(Result, 'scriptName')) = LowerCase(scriptName)) then begin
            exit;
        end;

        Result := getScript(e, scriptName);
    end;

    procedure processScriptLists(e: IInterface; scriptName: string; elementProps, pexProps: TJsonObject);
    var
        i: integer;
        curPropName, curType, curScriptName, curPropDisplayName: string;
        missingList, autoFillList: TJsonObject;
        script, baseObj: IInterface;

        isScalar, isAlias, isAutofilled, isMandatory: boolean;

    begin
        script := getScriptOrFragment(e, scriptName);
        if(not assigned(script)) then begin
            // is this a ref?
            if(isReferenceSignature(Signature(e))) then begin
                baseObj := pathLinksTo(e, 'NAME');
                script := getScriptOrFragment(baseObj, scriptName);                
            end;
        end;
        
        if(not assigned(script)) then begin
            AddMessage('Failed to find '+scriptName+' in the object again');
            exit;
        end;

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
                    if(attemptAutofill(script, curPropDisplayName, curType, isMandatory)) then begin
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
            if(curNameLC = '') then continue;
            curName := curList.O[curNameLC].S['name'];
            // AddMessage('checking #'+IntToStr(i)+' curname='+curName+' curNameLC='+curNameLC);

            curScriptList := curList.O[curNameLC];

            mandatoryList := getMandatoryProperties(curName);
            if(mandatoryList = nil) then continue;
//procedure processScriptLists(e: IInterface; scriptName: string; elementProps, pexProps: TJsonObject);
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