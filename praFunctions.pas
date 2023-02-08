{
    Just some functions for my scripts
    DEPRECATED. I should really get rid of this...
}
unit praFunctions;

    var
        overridesKeys: TStringList;
        overridesVals: TStringList;
        overridesIgnore: TStringList;

        listInitDone: boolean;
        kwListFood: TStringList;
        kwListFoodDisease: TStringList;
        kwListDrink: TStringList;
        kwListChem: TStringList;
        kwListDevice: TStringList;

        hcMedicineList: TStringList;

        effectsFoodWater: TStringList;
        effectsChem: TStringList;

        modelListDevice: TStringList;
        modelListPrewarFood: TStringList;
        modelListTool: TStringList;
        modelListMedical: TStringList;

        soundListFood: TStringList;
        soundListChem: TStringList;
        soundListDevice: TStringList;
        soundListTools: TStringList;

    //listInitDone := false;
    // --- OVERRIDE STUFF -----------------------------------------------------
    function extractVisTag(text: String): String;
    var
        i: integer;
        ending: integer;
        len: integer;
        c: String;
    begin
        len := length(text);

        Result := '';

        if (checkVisTag(text)) then begin
            for i := 2 to len do begin
                c := text[i];
                if(checkVisTag(c)) then begin
                    Result := copy(text, 0, i);
                    exit;
                end;
            end;
        end;
    end;

    function checkVisTag(text: String): boolean;
    var c: String;
    begin
        if text = '' then begin
            Result := false;
            exit;
        end;


        c := text[1];
        Result := true;
        if (c = '[') then exit;
        if (c = ']') then exit;
        if (c = '|') then exit;
        if (c = '{') then exit;
        if (c = '}') then exit;
        if (c = '(') then exit;
        if (c = ')') then exit;

        Result := false;


    end;

    function getCharPos(text: String; char: String): integer;
    var
        c: String;
        i, len: integer;

    begin
        len := length(text);

        Result := -1;


        for i := 1 to len do begin
            c := text[i];
            if(c = char) then begin
                Result := i;
                exit;
            end;
        end;

    end;

    procedure prepareOverrides();
    begin
        overridesKeys := TStringList.Create;
        overridesVals := TStringList.Create;
        overridesIgnore := TStringList.Create;

        overridesKeys.sorted := true;
    end;


    function showFileSelectionDialog(e: IInterface): IInterface;
    var
        i: integer;
        frm: TForm;
        clb: TCheckListBox;
        elemFileName, curFileName: string;
        curFile: IInterface;
    begin
        Result := nil;
        frm := frmFileSelect;
        elemFileName := LowerCase(GetFileName(e));
        try
            frm.Caption := 'Select a plugin';
            clb := TCheckListBox(frm.FindComponent('CheckListBox1'));
            clb.Items.Add('<new file>');
            for i := Pred(FileCount) downto 0 do begin
                curFile := FileByIndex(i);
                curFileName := GetFileName(curFile);

                if (elemFileName <> LowerCase(curFileName)) then begin
                    //AddMessage('Adding file '+curFileName);
                    clb.Items.InsertObject(1, curFileName, curFile);
                end else begin
                    Break;
                end;
            end;

            if (frm.ShowModal <> mrOk) then begin
                AddMessage('File Selection cancelled');
                Result := nil;
                // frm.Free; // finally still triggers
                Exit;
            end;

            for i := 0 to Pred(clb.Items.Count) do begin
                if clb.Checked[i] then begin
                    if i = 0 then begin
                        //AddMessage('Adding new file');
                        Result := AddNewFile();
                    end else begin
                        //AddMessage('Selected file at '+IntToStr(i));
                        Result := ObjectToElement(clb.Items.Objects[i]);
                    end;
                    Break;
                end;
            end;

        finally
            frm.Free;
        end;

        if(not assigned(Result)) then begin
            AddMessage('Something bad happened in showFileSelectionDialog');
        end;
    end;


    function GetOverride(key: String) : String;
    var
        i: integer;
    begin
        i := overridesKeys.indexOf(key);

        if i >= 0 then begin
            Result := overridesVals[i];
        end else begin
            Result := '';
        end;
    end;

    function IsItemIgnored(key: String): boolean;
    begin
        Result := overridesIgnore.indexOf(key) >= 0;
    end;


    procedure RegisterOverride(key: String; val: String);
    var
        i: integer;
    begin
        i := overridesKeys.indexOf(key);

        if i < 0 then begin
            i := overridesKeys.add(key);
            overridesVals.insert(i, val);
            exit;
        end else begin
            overridesVals[i] := val;
        end;
    end;

    procedure loadOverrides(filename: String);
    var
        list: TStringList;
        i, j: Integer;
        endPos: Integer;
        curLine: String;
        key: String;
        val: String;
        spacePos: Integer;
    begin
        list := TStringList.Create;
        list.loadFromFile(filename);

        for i:=0 to list.count-1 do begin
            curLine := trim(list[i]);
            if (length(curLine) > 0) then begin

                if (curLine[1] <> ';') then begin
                    spacePos := -1;
                    for j:=1 to length(curLine) do begin
                        if curLine[j] = '=' then begin
                            spacePos := j;
                            break;
                        end;
                        //AddMessage(curLine[j]);
                    end;
                    if spacePos <> -1 then begin
                        key := trim(copy(curLine, 0, spacePos-1));
                        val := trim(copy(curLine, spacePos+1, length(curLine)));
                        if (key <> '') and (val <> '') then begin
                            registerOverride(key, val);
                        end else if (key <> '') and (val  = '') then begin
                            // ignored
                            overridesIgnore.add(key);
                        end else begin
                            AddMessage(curLine+' Is not a valid override line in '+filename);
                        end;
                    end;
                end;
            end;
        end;

        list.free();

    end;

    // --- OVERRIDE STUFF END -------------------------------------------------

    // Finds a loaded file by string. Returns the IwbFile
    function findFile (name: String): IwbFile;
    begin
        Result := findFileWithError(name, false);
    end;

    // Finds a loaded file by string. Returns the IwbFile. Outputs an error message if file not found
    function findFileWithError (name: String; showError: boolean): IwbFile;
    var
        i: integer;
        curFile: IwbFile;
    begin
        Result := nil;
        for i := 0 to Pred(FileCount) do
        begin
            curFile := FileByIndex(i);
            if(GetFileName(curFile) = name) then begin
                Result := curFile;
                exit;
            end;
        end;

        if(showError) then begin
            AddMessage(name+' not found!');
        end;
    end;

    function findFileEspOrEsl(baseName: String; showError: boolean): IwbFile;
    var
        curFileName: String;
        i: integer;
        curFile: IwbFile;
    begin
        Result := nil;
        for i := 0 to Pred(FileCount) do
        begin
            curFile := FileByIndex(i);
            curFileName := GetFileName(curFile);
            if(curFileName = baseName+'.esp') or (curFileName = baseName+'.esl') then begin
                Result := curFile;
                exit;
            end;
        end;

        if(showError) then begin
            AddMessage('Found neither '+baseName+'.esp nor '+baseName+'.esl');
        end;
    end;

    function BoolToStr(b: boolean): string;
    begin
        if(b) then begin
            Result := 'True';
        end else begin
            Result := 'False';
        end;
    end;

    function ElementTypeString(e: IInterface): string;
    begin
      Result := '';
      if ElementType(e) = etFile then
        Result := 'etFile'
      else if ElementType(e) = etMainRecord then
        Result := 'etMainRecord'
      else if ElementType(e) = etGroupRecord then
        Result := 'etGroupRecord'
      else if ElementType(e) = etSubRecord then
        Result := 'etSubRecord'
      else if ElementType(e) = etSubRecordStruct then
        Result := 'etSubRecordStruct'
      else if ElementType(e) = etSubRecordArray then
        Result := 'etSubRecordArray'
      else if ElementType(e) = etSubRecordUnion then
        Result := 'etSubRecordUnion'
      else if ElementType(e) = etArray then
        Result := 'etArray'
      else if ElementType(e) = etStruct then
        Result := 'etStruct'
      else if ElementType(e) = etValue then
        Result := 'etValue'
      else if ElementType(e) = etFlag then
        Result := 'etFlag'
      else if ElementType(e) = etStringListTerminator then
        Result := 'etStringListTerminator'
      else if ElementType(e) = etUnion then
        Result := 'etUnion';
    end;

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

    procedure dumpElem(e: IInterface);
    begin
        dumpElement(e, '');
    end;



    function ElementByNamePath(e: IInterface; namePath: string): IInterface;
    var
        i, len: integer;
        c, curPart: String;
        curElem: IInterface;
    begin

        curPart := '';
        len := length(namePath);
        curElem := e;
        Result := nil;

        for i := 1 to len do begin
            c := copy(namePath, i, 1);
            if((c = '\')) then begin


                curElem := ElementByName(curElem, curPart);
                if(not assigned(curElem)) then begin
                    exit;
                end;
                //AddMessage('cur part: '+curPart);
                curPart := '';
            end else begin
                curPart := curPart + c;
            end;
        end;

        Result := ElementByName(curElem, curPart);
    end;


    { procedure addKeywordFFFOOOO(toElem: IInterface; kw: IInterface; targetSig: string); }
    { var }
        { container: IInterface; }
        { newElem: IInterface; }
        { num: integer; }
        { formId: LongWord; }
    { begin }
        { container := ElementBySignature(toElem, targetSig); }
        { num := ElementCount(container); }

        { if((not assigned(container)) or (num <= 0)) then begin }
            { container := Add(toElem, targetSig, True); }
        { end; }

        { newElem := ElementAssign(container, HighInteger, nil, False); }
        { formId := GetLoadOrderFormID(kw); }
        { SetEditValue(newElem, IntToHex(formId, 8)); }


    { end; }

    procedure addKeyword(toElem: IInterface; kw: IInterface; targetSig: string);
    var
        container: IInterface;
        newElem: IInterface;
        num: integer;
        formId: LongWord;
    begin
        container := ElementBySignature(toElem, targetSig);
        num := ElementCount(container);

        if((not assigned(container)) or (num <= 0)) then begin
            container := Add(toElem, targetSig, True);
        end;

        newElem := ElementAssign(container, HighInteger, nil, False);
        formId := GetLoadOrderFormID(kw);
        SetEditValue(newElem, IntToHex(formId, 8));


    end;

    function getFormlistEntryByEdid(formList: IInterface; edid: string): IInterface;
    var
        numElems, i : integer;
        curElem: IInterface;
        formIdList: IInterface;
    begin
        Result := nil;
        formIdList := ElementByName(formList, 'FormIDs');
        if(assigned(formIdList)) then begin
            numElems := ElementCount(formIdList);

            if(numElems > 0) then begin

                for i := 0 to numElems-1 do begin
                    curElem := LinksTo(ElementByIndex(formIdList, i));

                    if(GetElementEditValues(curElem, 'EDID') = edid) then begin
                        Result := curElem;
                        exit;
                    end;
                end;

            end;
        end;
    end;

    function hasFormlistEntry(formList: IInterface; entry: IInterface): boolean;
    var
        numElems, i : integer;
        curElem: IInterface;
        formIdList: IInterface;
    begin
        Result := false;
        formIdList := ElementByName(formList, 'FormIDs');
        if(assigned(formIdList)) then begin
            numElems := ElementCount(formIdList);

            if(numElems > 0) then begin

                for i := 0 to numElems-1 do begin
                    curElem := LinksTo(ElementByIndex(formIdList, i));

                    if(isSameForm(curElem, entry)) then begin
                        Result := true;
                        exit;
                    end;
                end;

            end;
        end;
    end;

    {
        Adds a form to a formlist, if it doesn't exist already
    }
    procedure addToFormlist(formList: IInterface; newForm: IInterface);
    var
        numElems, i : integer;
        curElem: IInterface;
        formIdList: IInterface;
    begin

        if(not assigned(newForm)) or (GetLoadOrderFormID(newForm) = 0) then begin
            exit;
        end;


        formIdList := ElementByName(formList, 'FormIDs');
        if(assigned(formIdList)) then begin
            numElems := ElementCount(formIdList);

            if(numElems > 0) then begin

                for i := 0 to numElems-1 do begin
                    curElem := LinksTo(ElementByIndex(formIdList, i));
                    if(isSameForm(curElem, newForm)) then begin
                        exit;
                    end;
                end;

            end;
        end else begin
            formIdList := Add(formList, 'FormIDs', True);
        end;

        curElem := ElementAssign(formIdList, HighInteger, nil, False);
        SetEditValue(curElem, IntToHex(GetLoadOrderFormID(newForm), 8));

    end;


    {
        If the FNAM is within kwsSearch, it is replaced by kwReplace
        @param cobj         the COBJ IInterface
        @param kwsSearch    list of kw names to replace
        @param kwReplace    replacement KW IInterface
        @param addIfNone    if true and the cobj has no FNAM, it will be added
    }
    procedure replaceAnyFnam(cobj: IInterface; kwsSearch: TStringList; kwReplace: IInterface; addIfNone: boolean);
    var
        fnam: IInterface;
        curKW: IInterface;
        curElem: IwbElement;
        newElem: IwbElement;
        i: integer;
        num: integer;
        formId: LongWord;
    begin
        fnam := ElementBySignature(cobj, 'FNAM');
        num := ElementCount(fnam);

        if(num > 0) then begin
            curElem := nil;
            for i := 0 to num-1 do begin
                curElem := ElementByIndex(fnam, i);
                curKW := LinksTo(curElem);

                //if(GetLoadOrderFormID(curKW) = GetLoadOrderFormID(kwSearch)) then begin

                if (kwsSearch.indexOf(GetElementEditValues(curKW, 'EDID')) >= 0) then begin
                    RemoveElement(fnam, curElem);
                    // add a
                    newElem := ElementAssign(fnam, HighInteger, nil, False);
                    formId := GetLoadOrderFormID(kwReplace);
                    SetEditValue(newElem, IntToHex(formId, 8));
                    break;
                end;

            end;

        end else begin
            if (addIfNone) then begin
                fnam := Add(cobj, 'FNAM', True);


                newElem := ElementAssign(fnam, HighInteger, nil, False);
                formId := GetLoadOrderFormID(kwReplace);
                SetEditValue(newElem, IntToHex(formId, 8));
            end;
        end;
    end;

    {
        If the FNAM is identical to kwSearch, it is replaced by kwReplace
        @param cobj         the COBJ IInterface
        @param kwSearch     the search KW IInterface
        @param kwReplace    replacement KW IInterface
        @param addIfNone    if true and the cobj has no FNAM, it will be added
    }
    procedure replaceFnam(cobj: IInterface; kwSearch: IInterface; kwReplace: IInterface; addIfNone: boolean);
    var
        fnam: IInterface;
        curKW: IInterface;
        curElem: IwbElement;
        newElem: IwbElement;
        i: integer;
        num: integer;
        formId: LongWord;
    begin
        fnam := ElementBySignature(cobj, 'FNAM');
        num := ElementCount(fnam);

        if(num > 0) then begin
            curElem := nil;
            for i := 0 to num-1 do begin
                curElem := ElementByIndex(fnam, i);
                curKW := LinksTo(curElem);

                //if(GetLoadOrderFormID(curKW) = GetLoadOrderFormID(kwSearch)) then begin
                if(isSameForm(curKW, kwSearch)) then begin
                    // replace
                    // remove curElem
                    RemoveElement(fnam, curElem);
                    // add a
                    newElem := ElementAssign(fnam, HighInteger, nil, False);
                    formId := GetLoadOrderFormID(kwReplace);
                    SetEditValue(newElem, IntToHex(formId, 8));
                    break;
                end;

            end;

        end else begin
            if (addIfNone) then begin

                fnam := Add(cobj, 'FNAM', True);


                newElem := ElementAssign(fnam, HighInteger, nil, False);
                formId := GetLoadOrderFormID(kwReplace);
                SetEditValue(newElem, IntToHex(formId, 8));
            end;
        end;
    end;

    function getFvpaCount(cobj: IInterface; itemForm: IInterface): integer;
    var
        fvpa: IInterface;
        curEntry: IInterface;
        curElem: IwbElement;
        i: integer;
        num: integer;
    begin
        fvpa := ElementBySignature(cobj, 'FVPA');
        num := ElementCount(fvpa);

        Result := 0;
        curElem := nil;
        for i := 0 to num-1 do begin
            curElem := ElementByIndex(fvpa, i);
            curEntry := LinksTo(ElementByName(curElem, 'Component'));

            if(isSameForm(itemForm, curEntry)) then begin
                Result := StrToInt(GetEditValue(ElementByName(curElem, 'Count')));
                exit;
            end;
        end;

    end;

    procedure setFvpaCount(cobj: IInterface; itemForm: IInterface; newCount: integer);
    var
        fvpa: IInterface;
        curEntry: IInterface;
        curElem: IwbElement;
        i: integer;
        num: integer;
        formId: LongWord;
    begin
        fvpa := ElementBySignature(cobj, 'FVPA');
        num := ElementCount(fvpa);


        curElem := nil;
        for i := 0 to num-1 do begin
            curElem := ElementByIndex(fvpa, i);
            curEntry := LinksTo(ElementByName(curElem, 'Component'));

            if(isSameForm(itemForm, curEntry)) then begin
                if(newCount <= 0) then begin
                    RemoveElement(fvpa, curElem);
                end else begin
                    SetEditValue(ElementByName(curElem, 'Count'), newCount);
                end;
                exit;
            end;
        end;

        if(newCount <= 0) then begin
            exit;
        end;

        curElem := ElementAssign(fvpa, HighInteger, nil, False);
        formId := GetLoadOrderFormID(itemForm);

        SetEditValue(ElementByName(curElem, 'Component'), IntToHex(formId, 8));
        SetEditValue(ElementByName(curElem, 'Count'), newCount);


    end;
    // struct processing functions
    function getStructMember(struct: IInterface; memberName: string): IInterface;
    var
        curElem: IInterface;
        cnt, i: integer;
    begin
        cnt := ElementCount(struct);
        result := nil;
        for i := 0 to cnt-1 do begin
            curElem := ElementByIndex(struct, i);
            if(GetElementEditValues(curElem, 'memberName') = memberName) then begin
                result := ElementByName(curElem, 'Value');
                exit;
            end;
        end;
    end;

    procedure addStructMemberScalar(struct: IInterface; memberName: string; memberType: string; value: string);
    var
        curMember: IInterface;
    begin
        curMember := ElementAssign(struct, HighInteger, nil, False);
        SetToDefault(curMember);
        SetElementEditValues(curMember, 'memberName', memberName);
        SetElementEditValues(curMember, 'Flags', 'Edited');
        SetElementEditValues(curMember, 'Type', memberType);
        SetElementEditValues(curMember, memberType, value);

    end;

    procedure addStructMemberLinkedObject(struct: IInterface; memberName: string; targetObject: IInterface);
    var
        curMember: IInterface;
        objectPart: IInterface;
    begin
        curMember := ElementAssign(struct, HighInteger, nil, False);

        SetElementEditValues(curMember, 'memberName', memberName);
        SetElementEditValues(curMember, 'Flags', 'Edited');
        SetElementEditValues(curMember, 'Type', 'Object');

        // SetElementEditValues(curMember, memberType, value);
        objectPart := ElementByNamePath(curMember, 'Value\Object Union\Object v2');

        SetElementEditValues(objectPart, 'FormID', IntToHex(GetLoadOrderFormID(targetObject), 8));
        SetElementEditValues(objectPart, 'Alias', 'None');

    end;

    // "new" property processing functions
    function getPropertyOfScript(e: IInterface; scriptName: string; propertyName: String): IInterface;
    var
        curScript, curWat, kwda: IInterface;
        i, j, k: integer;
        properties: IInterface;
        prop: IInterface;
    begin
        Result := nil;
        kwda := ElementByName(e, 'VMAD - Virtual Machine Adapter');

        for i := 0 to ElementCount(kwda)-1 do begin
            curScript := ElementByIndex(kwda, i);
            //AddMessage('FUCK'+GetElementEditValue(curScript));
            //if(GetElementEditValues(curScript, 'scriptName') = scriptName) then begin

                for j := 0 to ElementCount(curScript)-1 do begin
                    curWat := ElementByIndex(curScript, j);
                    if(GetElementEditValues(curWat, 'scriptName') = scriptName) then begin


                        // try stuff?
                        properties := ElementByName(curWat, 'Properties');


                        for k := 0 to ElementCount(properties)-1 do begin
                            prop := ElementByIndex(properties, k);

                            if(GetElementEditValues(prop, 'propertyName') = propertyName) then begin
                                Result := prop;
                                exit;
                            end;
                        end;
                    end;
                end;
            //end;
        end;
    end;

    function getPropVal(propObj: IInterface): string;
    var
        typeStr: String;
    begin
        typeStr := GetElementEditValues(propObj, 'Type');
        Result := GetElementEditValues(propObj, typeStr);
    end;

    function getPropertyOfScriptValue(e: IInterface; scriptName: string; propertyName: String): string;
    var
        propObj: IInterface;
        typeStr: String;
    begin
        propObj := getPropertyOfScript(e, scriptName, propertyName);

        Result := '';
        if(assigned(propObj)) then begin
            Result := getPropVal(propObj);
        end;
    end;

    procedure setPropertyOfScriptValue(e: IInterface; scriptName: string; propertyName: String; newValue: String);
    var
        propObj: IInterface;
        typeStr: String;
    begin
        propObj := getPropertyOfScript(e, scriptName, propertyName);
        if(assigned(propObj)) then begin
            typeStr := GetElementEditValues(propObj, 'Type');
            SetElementEditValues(propObj, typeStr, newValue);
        end;
    end;

    function extractScriptPropertyObject(e: IInterface): IInterface;
    var
        value: IInterface;
    begin
        Result := nil;

        value := ElementByNamePath(e, 'Value\Object Union\Object v2\FormID');
        if(not assigned(value)) then begin
            exit;
        end;

        Result := LinksTo(value);
    end;

    procedure replaceScriptPropertyObject(e: IInterface; newObj: IInterface);
    var
        value: IInterface;
        formId: LongWord;
    begin

        value := ElementByNamePath(e, 'Value\Object Union\Object v2\FormID');
        if(not assigned(value)) then begin
            exit;
        end;


        formId := GetLoadOrderFormID(newObj);

        SetEditValue(value, IntToHex(formId, 8));
    end;


    function getPropertyOfScriptLinkedObject(e: IInterface; scriptName: String; propertyName: String): IInterface;
    var
        prop: IInterface;
    begin
        prop := getPropertyOfScript(e, scriptName, propertyName);

        Result := extractScriptPropertyObject(prop);
    end;


    procedure setPropertyOfScriptLinkedObject(e: IInterface; scriptName: String; propertyName: String; newObject: IInterface);
    var
        prop: IInterface;
        value: IInterface;
        formId: LongWord;
    begin
        prop := getPropertyOfScript(e, scriptName, propertyName);
        replaceScriptPropertyObject(prop, newObject);
    end;


    function getScriptProperty(e: IInterface; propertyName: String): IInterface;
    var
        curScript, curWat, kwda: IInterface;
        i, j, k: integer;
        properties: IInterface;
        prop: IInterface;
    begin
        Result := nil;
        kwda := ElementByName(e, 'VMAD - Virtual Machine Adapter');

        for i := 0 to ElementCount(kwda)-1 do begin
            curScript := ElementByIndex(kwda, i);

            //addMessage('bar'+IntToStr(ElementCount(curScript)));

            for j := 0 to ElementCount(curScript)-1 do begin
                curWat := ElementByIndex(curScript, j);

                // try stuff?
                properties := ElementByName(curWat, 'Properties');


                for k := 0 to ElementCount(properties)-1 do begin
                    prop := ElementByIndex(properties, k);

                    if(GetElementEditValues(prop, 'propertyName') = propertyName) then begin
                        Result := prop;
                        exit;
                    end;
                end;
            end;
        end;
    end;

    function getPropertyValue(e: IInterface): IInterface;
    var
        ty: String;
        value: IInterface;
        iterable: IInterface;
    begin
        Result := nil;
        ty := GetElementEditValues(e, 'Type');
        value := ElementByName(e, 'Value');

        Result := ElementByName(value, ty);
    end;

    function getPropertyValueAt(e: IInterface; index: integer): IInterface;
    var
        i: integer;
        ty: String;
        value: IInterface;
        iterable: IInterface;
    begin
        iterable := getPropertyValue(e);

        if (index >= ElementCount(iterable)) then begin
            Result := nil;
        end else begin
            Result := ElementByIndex(iterable, index);
        end;
    end;


    function getScriptPropertyScalar(e: IInterface; propertyName: String; defVal: String): String;
    var
        propObj: IInterface;
        typeStr: String;
    begin
        propObj := getScriptProperty(e, propertyName);

        Result := defVal;
        if(assigned(propObj)) then begin
            typeStr := GetElementEditValues(propObj, 'Type');
            Result := GetElementEditValues(propObj, typeStr);
        end;
    end;



    function getScriptPropertyInt(e: IInterface; propertyName: String; defVal: integer): integer;
    begin
        Result := StrToInt(getScriptPropertyScalar(e, propertyName, IntToStr(defVal)));
    end;

    {
        Checks if the two objects are the same, because IInterfaces aren't comparable
    }
    function isSameForm(e1: IInterface; e2: IInterface): boolean;
    begin
        if ((not assigned(e1)) or (not assigned(e2))) then begin
            if ((not assigned(e1)) and (not assigned(e2))) then begin
                Result := true;
                exit;
            end;
            Result := false;
            exit;
        end;
        Result := (GetLoadOrderFormID(e1) = GetLoadOrderFormID(e2));
    end;

    {
        Returns the object crafted by this
        @param e    must be a COBJ
    }
    function getCraftResult(e: IInterface): IInterface;
    var
        cnam: IInterface;
    begin
        // cnam := ElementByName(e, 'CNAM - Created Object');
        cnam := ElementBySignature(e, 'CNAM');
        if(assigned(cnam)) then begin
            Result := LinksTo(cnam);
        end;

    end;

    procedure setBnam(e: IInterface; keyword: IInterface);
    var
        formId: LongWord;
        enit: IInterface;
    begin
        formId := GetLoadOrderFormID(keyword);

        SetEditValue(ElementBySignature(e, 'BNAM'), IntToHex(formId, 8));
    end;

    function getWorkbenchType(e: IInterface): string;
    var
        wbdt: IInterface;
    begin
        wbdt := ElementBySignature(e, 'WBDT');
        if(assigned(wbdt)) then begin
            Result := GetElementEditValues(wbdt, 'Bench Type');
            //dumpElement(wbdt, '');
            //AddMessage('result='+result);
        end;
    end;

    function getBnam(e: IInterface): IInterface;
    var
        bnam: IInterface;
    begin
        // cnam := ElementByName(e, 'CNAM - Created Object');
        bnam := ElementBySignature(e, 'BNAM');
        if(assigned(bnam)) then begin
            Result := LinksTo(bnam);
        end;

    end;

    function getActorValuePropertyByName(e: IInterface; valName: String): float;
    var
        root: IInterface;
        cur: IInterface;
        av: IInterface;
        i: Integer;

        test : string;
    begin
        Result := 0;

        root := ElementBySignature(e, 'PRPS');

        for i := 0 to ElementCount(root)-1 do begin
            cur := ElementByIndex(root, i);
            av := LinksTo(ElementByName(cur, 'Actor Value'));



            if(GetElementEditValues(av, 'EDID') = valName) then begin
                Result := StrToFloat(GetElementEditValues(cur, 'Value'));
                exit;
            end;
        end;
    end;


    function getActorValueProperty(e: IInterface; valName: String): float;
    begin
        Result := getActorValuePropertyByName(e, valName);
    end;

    function getActorValuePropertyByProp(e: IInterface; prop: IInterface): float;
    var
        root: IInterface;
        cur: IInterface;
        av: IInterface;
        i: Integer;
    begin
        Result := 0;

        root := ElementBySignature(e, 'PRPS');

        for i := 0 to ElementCount(root)-1 do begin
            cur := ElementByIndex(root, i);
            av := LinksTo(ElementByName(cur, 'Actor Value'));

            if(isSameForm(av, prop)) then begin
                Result := StrToFloat(GetElementEditValues(cur, 'Value'));
                exit;
            end;
        end;
    end;

    procedure setActorValueProperty(e: IInterface; prop: IInterface; newValue: float);
    var
        root: IInterface;
        cur: IInterface;
        av: IInterface;
        i: Integer;
        newElem: IInterface;
        formId: LongWord;
    begin

        root := ElementBySignature(e, 'PRPS');

        for i := 0 to ElementCount(root)-1 do begin
            cur := ElementByIndex(root, i);
            av := LinksTo(ElementByName(cur, 'Actor Value'));

            if(isSameForm(av, prop)) then begin
                setElementEditValues(cur, 'Value', floattostr(newValue));
                // Result := StrToFloat(GetElementEditValues(cur, 'Value'));
                exit;
            end;
        end;

        // add value
        newElem := ElementAssign(root, HighInteger, nil, False);
        formId := GetLoadOrderFormID(prop);
        setElementEditValues(newElem, 'Actor Value', IntToHex(formId, 8));
        setElementEditValues(newElem, 'Value', floattostr(newValue));
    end;

    {
        Checks if given object has the given keyword
        @param e            the object
        @param kw           the keyword's editor ID
        @param signature    the signature. Usually KWDA, but sometimes others
    }
    function hasKeyword(e: IInterface; kw: String; signature: String): boolean;
    var
        kwda: IInterface;
        curKW: IInterface;
        i: Integer;
    begin
        Result := hasKeywordBySignature(e, kw, signature);
    end;

    function hasKeywordBySignature(e: IInterface; kw: String; signature: String): boolean;
    var
        kwda: IInterface;
        curKW: IInterface;
        i: Integer;
    begin
        Result := false;
        //kwda := ElementByName(e, 'KWDA - Keywords');
        kwda := ElementBySignature(e, signature);


        for i := 0 to ElementCount(kwda)-1 do begin
            curKW := LinksTo(ElementByIndex(kwda, i));

            if GetElementEditValues(curKW, 'EDID') = kw then begin
                Result := true;
                exit;
            end
        end;
    end;

    procedure removeKeyword(e: IInterface; kw: String; signature: String);
    var
        kwda: IInterface;
        curKW: IInterface;
        i: Integer;
    begin
        kwda := ElementBySignature(e, signature);


        for i := 0 to ElementCount(kwda)-1 do begin
            curKW := LinksTo(ElementByIndex(kwda, i));

            if GetElementEditValues(curKW, 'EDID') = kw then begin
                RemoveElement(kwda, curKW);
                exit;
            end
        end;
    end;


    function hasKeywordIInterface(e: IInterface; kw: IInterface; signature: String): boolean;
    var
        kwda: IInterface;
        curKW: IInterface;
        i: Integer;
    begin
        Result := false;
        //kwda := ElementByName(e, 'KWDA - Keywords');
        kwda := ElementBySignature(e, signature);


        for i := 0 to ElementCount(kwda)-1 do begin
            curKW := LinksTo(ElementByIndex(kwda, i));

            if isSameForm(curKw, kw) then begin
                Result := true;
                exit;
            end;
        end;
    end;

    {
        Checks if given object has any of the given keywords
        @param e            the object
        @param kws          a stringlist of keyword editor IDs. Will return true if e has any of those
        @param signature    the signature. Usually KWDA, but sometimes others
    }
    function hasAnyKeyword(e: IInterface; kws: TStringList; signature: String): boolean;
    var
        kwda: IInterface;
        curKW: IInterface;
        i: Integer;
        kw: String;

    begin
        Result := false;
        //kwda := ElementByName(e, 'KWDA - Keywords');
        kwda := ElementBySignature(e, signature);


        for i := 0 to ElementCount(kwda)-1 do begin
            curKW := LinksTo(ElementByIndex(kwda, i));

            kw := GetElementEditValues(curKW, 'EDID');
            if(kws.indexOf(kw) >= 0) then begin
                Result := true;
                exit;
            end;

        end;
    end;

    {
        Checks if string haystack starts with string needle
    }
    function startsWith(needle: String; haystack: String): boolean;
    var
        len: Integer;
        cmp: String;
    begin
        if needle = haystack then begin
            Result := true;
            exit;
        end;

        len := length(needle);

        if len > length(haystack) then begin
            Result := false;
            exit;
        end;

        cmp := copy(haystack, 0, len);

        Result := (cmp = needle);
    end;

    function findFile (name: String): IwbFile;
    var
        i: integer;
        curFile: IwbFile;
    begin
        Result := nil;
        for i := 0 to Pred(FileCount) do
        begin
            curFile := FileByIndex(i);
            //AddMessage('file '+GetFileName(curFile));
            if(GetFileName(curFile) = name) then begin
                Result := curFile;
                exit;
            end;
        end;
    end;

    function getModelName(e: IInterface): string;
    var
        tempElem: IInterface;
    begin
        Result := '';
        tempElem := ElementByName(e, 'Model');
        if(assigned(tempElem)) then begin
            tempElem := ElementBySignature(tempElem, 'MODL');
            if(assigned(tempElem)) then begin
                Result := GetEditValue(tempElem);
            end;
        end;
    end;

    function setModelName(e: IInterface; newModel: String): boolean;
    var
        model: IInterface;
        MODL: IInterface;
        tempElem: IInterface;
    begin
        Result := false;
        model := ElementByName(e, 'Model');

        if(not assigned(model)) then begin
            model := Add(e, 'Model', True);
        end;

        //if(assigned(tempElem)) then begin
            MODL := ElementBySignature(model, 'MODL');

            if(not assigned(MODL)) then begin
                MODL := Add(model, 'MODL', True);
            end;
            //if(assigned(tempElem)) then begin
                SetEditValue(MODL, newModel);
                Result := true;
            //end;
        //end;
    end;

    function hasAnyScript(e: IInterface; scriptList: TStringList): boolean;
    var
        kwda: IInterface;
        curScript, curWat: IInterface;
        i, j: Integer;
    begin
        Result := false;
        kwda := ElementByName(e, 'VMAD - Virtual Machine Adapter');

        for i := 0 to ElementCount(kwda)-1 do begin
            curScript := ElementByIndex(kwda, i);


            for j := 0 to ElementCount(curScript)-1 do begin
                curWat := ElementByIndex(curScript, j);

                //AddMessage('FOO '+GetElementEditValues(curWat, 'scriptName')+' vs '+kw);

                if(scriptList.indexOf(GetElementEditValues(curWat, 'scriptName')) >= 0) then begin
                    Result := true;
                    exit;
                end;
            end;
        end;
    end;

    function hasScriptMulti(e: IInterface; kw: String; useStartsWith: boolean): boolean;
    var
        kwda: IInterface;
        curScript, curWat: IInterface;
        i, j: Integer;
        curScriptName, curSearchFor: string;
    begin
        Result := false;
        kwda := ElementByName(e, 'VMAD - Virtual Machine Adapter');

        curSearchFor := LowerCase(kw);

        for i := 0 to ElementCount(kwda)-1 do begin
            curScript := ElementByIndex(kwda, i);


            for j := 0 to ElementCount(curScript)-1 do begin
                curWat := ElementByIndex(curScript, j);

                curScriptName := LowerCase(GetElementEditValues(curWat, 'scriptName'));

                if (useStartsWith) then begin
                    if startsWith(curSearchFor, curScriptName) then begin
                        Result := true;
                        exit;
                    end;
                end else begin
                    if curScriptName = curSearchFor then begin
                        Result := true;
                        exit;
                    end;
                end;


            end;
        end;
    end;

    procedure initLists();
    begin
        if(listInitDone) then exit;

        listInitDone := true;


        kwListFood := TStringList.create;
        kwListFood.add('FoodEffect');
        {
        kwListFood.add('HC_DiseaseRisk_FoodVeryHigh');
        kwListFood.add('HC_DiseaseRisk_FoodLow');
        kwListFood.add('HC_DiseaseRisk_FoodHigh');
        kwListFood.add('HC_DiseaseRisk_FoodStandard');
        }
        kwListFood.add('FruitOrVegetable');
        kwListFood.add('ObjectTypeFood');

        effectsChem := TStringList.create;
        effectsChem.add('RestoreHealthChem');

        effectsFoodWater := TStringList.create;
        effectsFoodWater.add('RestoreHealthFood');

        kwListFoodDisease := TStringList.create;
        kwListFoodDisease.add('HC_DiseaseRisk_FoodVeryHigh');
        kwListFoodDisease.add('HC_DiseaseRisk_FoodLow');
        kwListFoodDisease.add('HC_DiseaseRisk_FoodHigh');
        kwListFoodDisease.add('HC_DiseaseRisk_FoodStandard');

        kwListDrink := TStringList.create;
        kwListDrink.add('AnimFurnWater');
        kwListDrink.add('ObjectTypeWater');
        kwListDrink.add('ObjectTypeDrink');
        kwListDrink.add('HC_SustenanceType_QuenchesThirst');
        kwListDrink.add('ObjectTypeCaffeinated');

        kwListChem := TStringList.create;

        kwListChem.add('ObjectTypeStimpak');
        kwListChem.add('ObjectTypeChem');
        kwListChem.add('CA_ObjType_ChemBad');
        kwListChem.add('HC_CausesImmunodeficiency');
        kwListChem.add('HC_SustenanceType_IncreasesHunger');

        hcMedicineList := TStringList.create;
        hcMedicineList.add('HC_Herbal_Anodyne');
        hcMedicineList.add('HC_Herbal_Antimicrobial');
        hcMedicineList.add('HC_Herbal_Stimulant');

        modelListPrewarFood := TStringList.create;
        modelListPrewarFood.add('Props\FoodandFoodware\MoldyFood01.nif');
        modelListPrewarFood.add('SetDressing\FoodAndFoodWare\LongneckCans.nif');

        modelListMedical := TStringList.create;
        modelListMedical.add('Props\GlowingOneBloodPack.nif');
        modelListMedical.add('Props\BloodPack.nif');
        //

        modelListDevice := TStringList.create;
        modelListDevice.add('Props\StealthBoy01.nif');
        modelListDevice.add('DLC01\Props\DLC01_RobotRepairKit01.nif');
        modelListDevice.add('Props\BS101RadioTransmittor.nif');
        modelListDevice.add('Props\BoSDistressPulser\BoSDistressPulserGlowing.nif');
        modelListDevice.add('Props\BoSCerebroFusionAdaptor\BoSCerebroFusionAdaptor.nif');
        modelListDevice.add('Actors\LibertyPrime\CharacterAssets\Agitator.nif');
        modelListDevice.add('Props\BoSDistressPulser\BoSDistressPulser.nif');
        modelListDevice.add('Props\BoSReflexCapacitor\BoSReflexCapacitor.nif');
        modelListDevice.add('Props\BoSHapticDrive\BoSHapticDrive.nif');
        modelListDevice.add('Props\BoSFluxSensor\BoSFluxSensor.nif');
        modelListDevice.add('Props\GenericKeycard01.nif');
        modelListDevice.add('Props\MilitaryCircuitBoard\MilitaryCircuitBoard.nif');
        modelListDevice.add('Props\PreWar_Toaster.nif');
        modelListDevice.add('Props\PostWar_Toaster.nif');
        modelListDevice.add('SetDressing\Building\DeskFanOffice01.nif');
        modelListDevice.add('SetDressing\Building\DeskFanOfficeOff01.nif');
        modelListDevice.add('DLC01\Props\DLC01_ComponentPart.nif');
        modelListDevice.add('DLC01\Props\DLC01_ComponentWhole.nif');
        modelListDevice.add('DLC01\Props\DLC01_Amplifier01.nif');
        modelListDevice.add('Props\SynthChip\SynthChip.nif');
        modelListDevice.add('DLC03\Props\DLC03_FogCondenserPowerModule.nif');
        modelListDevice.add('Props\MS11PowerRelayCoil.nif');
        modelListDevice.add('Props\Component\Component_Circuitry.nif');
        modelListDevice.add('Props\BioMedicalScanner\BioMedicalScanner.nif');
        modelListDevice.add('Props\Camera.nif');
        modelListDevice.add('Props\Fuse01.nif');
        modelListDevice.add('Props\FusionPulseCharge\FusionPulseCharge.nif');
        modelListDevice.add('Props\BOS_Magnet.nif');
        modelListDevice.add('Props\HotPlate.nif');
        modelListDevice.add('Props\MS11RadarTransmittor.nif');
        modelListDevice.add('SetDressing\LightFixtures\LightbulbOff.nif');
        modelListDevice.add('Props\Chipboard.nif');
        modelListDevice.add('SetDressing\Quest\GenPrototype01.nif');
        modelListDevice.add('Props\VacuumTube01.nif');
        modelListDevice.add('Props\StealthBoy01.nif');
        modelListDevice.add('Weapons\Grenade\TransmitGrenadeProjectile.nif');
        modelListDevice.add('Props\PipboyMiscItem\PipboyMisc01.nif');
        modelListDevice.add('DLC06\Props\PipboyMiscItem\DLC06PipboyMisc01.nif');
        modelListDevice.add('SetDressing\Factions\Railroad\TinkerTomsDevice01.nif');

        modelListTool := TStringList.create;
        modelListTool.add('AutoBuildPlots\Weapons\Hammer\Hammer.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolHammer01A.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolHammer01B.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolHammer02.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolHammer03.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolSaw01.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolSaw02.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolTongs01.nif');
        modelListTool.add('Props\SurgicalTools\SurgicalCutter.nif');
        modelListTool.add('Props\SurgicalTools\SurgicalScalpel.nif');
        modelListTool.add('Props\SurgicalTools\SurgicalScissors.nif');
        modelListTool.add('Props\BlowTorch.nif');
        modelListTool.add('Props\BlowTorch_Rare.nif');
        modelListTool.add('SS2\Props\ASAMTop_Moveable.nif');
        modelListTool.add('SS2\Props\ASAMSensor_Wall.nif');

        soundListFood := TStringList.create;

        soundListFood.add('NPCHumanEatChewy');
        soundListFood.add('NPCHumanEatGeneric');
        soundListFood.add('NPCHumanEatEgg');
        soundListFood.add('NPCHumanEatSoup');
        soundListFood.add('NPCHumanEatSoupSlurp');

        soundListChem := TStringList.create;
        soundListChem.add('NPCHumanEatMentats');
        soundListChem.add('NPCHumanChemsPsycho');
        soundListChem.add('NPCHumanChemsUseJet');
        soundListChem.add('NPCHumanChemsAddictol');

        soundListDevice := TStringList.create;
        soundListDevice.add('OBJStealthBoyActivate');

        soundListTools := TStringList.create;
        soundListTools.add('NPCHumanWhistleDog');

        kwListDevice := TStringList.create;
        kwListDevice.add('ChemTypeStealthBoy');
        kwListDevice.add('StealthBoyKeyword');
        kwListDevice.add('DLC01ObjectTypeRepairKit');


    end;

    procedure cleanUpLists();
    begin
        if(not listInitDone) then exit;
        effectsFoodWater.free();
        effectsChem.free();
        kwListFood.free();
        kwListFoodDisease.free();
        kwListDrink.free();
        kwListChem.free();
        modelListDevice.free();
        modelListTool.free();
        soundListFood.free();
        soundListChem.free();
        soundListDevice.free();
        soundListTools.free();
        kwListDevice.free();

        hcMedicineList.free();
        modelListMedical.free();

        modelListPrewarFood.free();
    end;

    procedure removeEffect(e: IInterface; effectName: String);
    var
        effects: IInterface;
        curEff, curWat: IInterface;
        i: Integer;
    begin
        effects := ElementByName(e, 'Effects');

        i := 0;

        while i < ElementCount(effects) do begin
            curEff := ElementByIndex(effects, i);

            curWat := LinksTo(ElementBySignature(curEff, 'EFID'));

            if(GetElementEditValues(curWat, 'EDID') = effectName) then begin
                RemoveElement(effects, curEff);
            end else begin
                i := i+1;
            end;
        end;

    end;

    procedure removeEffects(e: IInterface; effectNames: TStringList);
    var
        effects: IInterface;
        curEff, curWat: IInterface;
        i: Integer;
    begin
        // for now
        for i := 0 to effectNames.count-1 do begin
            removeEffect(e, effectNames[i]);
        end;
        { effects := ElementByName(e, 'Effects'); }

        { for i := 0 to ElementCount(effects)-1 do begin }
            { curEff := ElementByIndex(effects, i); }

            { curWat := LinksTo(ElementBySignature(curEff, 'EFID')); }

            { if(GetElementEditValues(curWat, 'EDID') = effectName) then begin }
                { RemoveElement(effects, curEff); }
                { exit; }
            { end; }
        { end; }
    end;

    function hasAnyEffect(e: IInterface; effectNames: TStringList): boolean;
    var
        effects: IInterface;
        curEff, curWat: IInterface;
        i: Integer;
    begin
        Result := false;
        effects := ElementByName(e, 'Effects');

        for i := 0 to ElementCount(effects)-1 do begin
            curEff := ElementByIndex(effects, i);
            curWat := LinksTo(ElementBySignature(curEff, 'EFID'));


            if(effectNames.indexOf(GetElementEditValues(curWat, 'EDID')) >= 0) then begin

                Result := true;
                exit;
            end;
        end;
    end;

    function hasEffect(e: IInterface; effectName: String): boolean;
    var
        effects: IInterface;
        curEff, curWat: IInterface;
        i: Integer;
    begin
        Result := false;
        effects := ElementByName(e, 'Effects');

        for i := 0 to ElementCount(effects)-1 do begin
            curEff := ElementByIndex(effects, i);

            curWat := LinksTo(ElementBySignature(curEff, 'EFID'));

            if(GetElementEditValues(curWat, 'EDID') = effectName) then begin
                Result := true;
                exit;
            end;
        end;
    end;

    function getFoodType(e: IInterface): integer;
    var
        doesRadDamage: boolean;
        modelName: string;
    begin
        modelName := getModelName(e);
        if (modelListPrewarFood.indexOf(modelName) >= 0) then begin
            Result := 12;
            exit;
        end;

        doesRadDamage := hasEffect(e, 'DamageRadiationChem');

        if(doesRadDamage) then begin

            if(hasKeywordBySignature(e, 'FruitOrVegetable', 'KWDA')) then begin
                Result := 13; // crop
                exit;
            end;

            if(hasKeywordBySignature(e, 'HC_IgnoreAsFood', 'KWDA')) then begin
                Result := 13; // crop
                exit;
            end;

            // HC_IgnoreAsFood [KYWD:00249F52]

            if(hasAnyKeyword(e, kwListFoodDisease, 'KWDA')) then begin
                Result := 11; // raw
                exit;
            end;

            // prewar?
            Result := 12;
            exit;
        end;

        Result := 10;
    end;

    {
        checks if the given ALCH is a device
        we have:
            - unknown       = 0
            - chem
                - good      = 1
                - bad       = 2
            - food
                - food        = 10
                - raw         = 11
                - prewar rads = 12
                - crop        = 13
            - drink
                - other     = 20
                - liquor    = 21
                - nuka      = 22
            - syringe       = 30
            - device
                - device    = 40
                - tool      = 41

    }
    function getAlchemyType(e: IInterface): integer;
    var
        flagsVal: integer;
        hasMedicineFlag: boolean;
        enit: IInterface;
        tempElem: IInterface;
        flags: IInterface;
        modelStr: String;
        soundName: String;
        isAddictive: boolean;
        addiction: IInterface;
        curElem : IInterface;
    begin

        Result := 0;
        initLists();

        // simple stuff which uses only one KW

        if(hasKeywordBySignature(e, 'ObjectTypeSyringerAmmo', 'KWDA')) then begin
            Result := 30; // syringe
            exit;
        end;

        if(hasKeywordBySignature(e, 'ObjectTypeNukaCola', 'KWDA')) then begin
            Result := 22; //nuka
            exit;
        end;

        if(hasKeywordBySignature(e, 'ObjectTypeAlcohol', 'KWDA')) then begin
            Result := 21; // alcohol
            exit;
        end;




        if(hasKeywordBySignature(e, 'ObjectTypeFood', 'KWDA')) then begin
            Result := getFoodType(e);
            exit;
        end;



        if(hasAnyKeyword(e, kwListFood, 'KWDA')) then begin
            Result := getFoodType(e);
            exit;
        end;

        if(hasAnyKeyword(e, kwListDrink, 'KWDA')) then begin
            Result := 20; // drink
            exit;
        end;

        if( hasAnyKeyword(e, kwListDevice, 'KWDA') ) then begin
            Result := 40;
            exit;
        end;

        // this now needs addiction data
        isAddictive := false;


        enit := ElementBySignature(e, 'ENIT');
        if enit <> nil then begin
            //flags := ElementByName(enit, 'Flags');
            addiction := ElementByName(enit, 'Addiction');
            if addiction <> nil then begin
                addiction := LinksTo(addiction);
            end;

            isAddictive := addiction <> nil;


            curElem := ElementByName(enit, 'Sound - Consume');
            if curElem <> nil then begin
                curElem := LinksTo(curElem);
                soundName := GetElementEditValues(curElem, 'EDID');
            end;
        end;



        if(hcMedicineList.indexOf(EditorID(e)) >= 0) then begin
            if(isAddictive) then begin
                Result := 2; //bad chem
            end else begin
                Result := 1; //good chem
            end;
            exit;
        end;

        if(hasAnyKeyword(e, kwListChem, 'KWDA')) then begin
            if(isAddictive) then begin
                Result := 2; //bad chem
            end else begin
                Result := 1; //good chem
            end;
            exit;
        end;


        // still alive here? try the model
        modelStr := getModelName(e);
        if(modelStr <> '') then begin
            if(modelListDevice.indexOf(modelStr) >= 0) then begin
                Result := 40; // device
                exit;
            end;

            if(modelListTool.indexOf(modelStr) >= 0) then begin
                Result := 41; // tool
                exit;
            end;

            if(modelListMedical.indexOf(modelStr) >= 0) then begin
                if(isAddictive) then begin
                    Result := 2; //bad chem
                end else begin
                    Result := 1; //good chem
                end;
                exit;
            end;
        end;

        // try the sound...
        tempElem := ElementByName(enit, 'Sound - Consume');
        if tempElem <> nil then begin
            tempElem := LinksTo(tempElem);
            soundName := GetElementEditValues(tempElem, 'EDID');

            if(soundName = 'NPCHumanDrinkGeneric') then begin
                if(isAddictive) then begin
                    Result := 21; // alcohol
                end else begin
                    Result := 20; // drink
                end;
                exit;
            end;


            if(soundListFood.indexOf(soundName) >= 0) then begin

                Result := getFoodType(e);
                exit;
            end;

            if(soundListChem.indexOf(soundName) >= 0) then begin
                if(isAddictive) then begin
                    Result := 2; // bad chem
                end else begin
                    Result := 1; // good chem
                end;
                exit;
            end;

            if(soundListDevice.indexOf(soundName) >= 0) then begin
                Result := 40;
                exit;
            end;

            if(soundListTools.indexOf(soundName) >= 0) then begin
                Result := 41;
                exit;
            end;
        end;

        // try effects
        if(hasAnyEffect(e, effectsChem)) then begin
            // RestoreHealthChem
            if(isAddictive) then begin
                Result := 21; // alcohol
            end else begin
                Result := 20; // drink
            end;
        end;
        if(hasAnyEffect(e, effectsFoodWater)) then begin
            // either food or water.. now how tell them apart?
            // I hope if it was water, it was caught above
            Result := getFoodType(e);
            exit;
        end;

        if(hasAnyKeyword(e, kwListFoodDisease, 'KWDA')) then begin
            // as above
            Result := 11; // raw food
            exit;
        end;

        // get more data
        hasMedicineFlag := false;
        flags := ElementByNamePath(enit, 'Flags\Medicine');

        if(assigned(flags)) then begin
            flagsVal := GetEditValue(flags);
            if(flagsVal = '1') then begin
                hasMedicineFlag := true;
            end;
        end;


        // no idea... use that flag
        if(hasMedicineFlag) then begin
            Result := 1;
        end;

        // no idea
        Result := 0;
    end;

    // special function in order to prevent multiple spaces, or stuff like '[prefix] - Asd'
    function prefixTag(tag: string; name: string): string;
    var
        nameCopy, cleanName: String;
        char: String;
        i: integer;
    begin
        nameCopy := name;
        cleanName := name;
        // Result := name;
        for i := 1 to length(nameCopy) do begin
            char := nameCopy[i];
            if not ((char = ' ') or (char = '-')) then begin
                cleanName := copy(nameCopy, i, length(nameCopy));

                break;
            end;
        end;

        Result := tag + ' ' + cleanName;

    end;

    function findExistingOverrideInFile(sourceElem: IInterface; targetFile: IwbFile): IInterface;
    var
        masterElem, curOverride: IINterface;
        numOverrides, i: integer;
        targetFileName: string;
    begin
        Result := nil;

        masterElem := MasterOrSelf(sourceElem);
        targetFileName := GetFileName(targetFile);

        numOverrides := OverrideCount(masterElem);

        for i:=0 to numOverrides-1 do begin
            curOverride := OverrideByIndex(masterElem, i);

            if(GetFileName(GetFile(curOverride)) = targetFileName) then begin
                Result := curOverride;
                exit;
            end;

        end;

    end;

    function getLatestOverrideExceptFile(sourceElem: IInterface; targetFile: IwbFile): IInterface;
    var
        i, numOverrides, targetFileIndex, curFileIndex: integer;
        theMaster, curOverride, curFile: IInterface;
    begin
        theMaster := MasterOrSelf(sourceElem);
        Result := theMaster;
        numOverrides := OverrideCount(theMaster);
        if(numOverrides = 0) then begin
            exit;
        end;


        targetFileIndex := GetLoadOrder(targetFile);

        for i:=0 to numOverrides-1 do begin
            curOverride := OverrideByIndex(theMaster, i);
            curFile := GetFile(curOverride);


            curFileIndex := GetLoadOrder(curFile);

            if (curFileIndex >= targetFileIndex) then begin
                exit;
            end;

            Result := curOverride;
        end;
    end;

    // this should either generate an override for given element, or fetch it if it already exists
    function getOverrideForElem(sourceElem: IInterface; targetFile: IwbFile): IInterface;
    var
        existingOverride: IInterface;
    begin
        sourceElem := getLatestOverrideExceptFile(sourceElem, targetFile);

        existingOverride := findExistingOverrideInFile(sourceElem, targetFile);

        if(assigned(existingOverride)) then begin
            Remove(existingOverride);
        end;


        //
        AddRequiredElementMasters(sourceElem, targetFile, False);
        Result := wbCopyElementToFile(sourceElem, targetFile, False, True);
    end;

    function hasFlag(e: IInterface; flagName: string): boolean;
    var
        i: integer;
        curName, curValue: string;
    begin
        Result := false;

        for i:=0 to ElementCount(e)-1 do begin
            curName := DisplayName(ElementByIndex(e, i));
            curValue := GetEditValue(ElementByIndex(e, i));
            if (curName = flagName) and (curValue = '1') then begin
                Result := true;
                exit;
            end;
        end
    end;

    {
        Replaces an element with a copy of another. Or just copies that element.

        @param edidToReplace    editor ID of the element to replace
        @param sourceElem       element to copy over
        @param targetFile       target file where to put the copy
    }
    function replaceElementWithCopy(edidToReplace: string; sourceElem: IInterface; targetFile: IwbFile): IInterface;
    var
        existingVersion: IInterface;
        signature: string;
    begin
        signature := signature(sourceElem);

        existingVersion := MainRecordByEditorID(GroupBySignature(targetFile, signature), edidToReplace);

        if(assigned(existingVersion)) then begin
            Remove(existingVersion);
        end;

        AddRequiredElementMasters(sourceElem, targetFile, False);
        Result := wbCopyElementToFile(sourceElem, targetFile, True, True);
    end;

    function getUnixTimestamp(): Integer;
    begin
        // shamelessly stolen from stackoverflow
        // https://stackoverflow.com/questions/4420188/how-to-format-a-unix-timestamp-in-delphi
        Result := Round((Now - 25569) * 86400);
    end;
end.