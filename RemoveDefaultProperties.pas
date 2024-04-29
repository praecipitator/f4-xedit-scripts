{
    Strips script properties according to rules defined in default-value-config.txt
}
unit RemoveDefaultProperties;
    const
        configFile = ScriptsPath + 'default-value-config.txt';
    var
        // couldn't figure out how to do it with just one...
        configKeys: TStringList;
        configValues: TStringList;
        
    function HasConfig(key: String) : boolean;
    begin
        Result := (configKeys.indexOf(key) >= 0);
    end;
    
    function GetConfig(key: String) : String;
    var
        i: integer;
    begin
        i := configKeys.indexOf(key);
        
        if i >= 0 then begin
            Result := configValues[i];
        end else begin
            Result := '';
        end;        
    end;


    procedure SetConfig(key: String; val: String);
    var
        i: integer;
    begin
        i := configKeys.indexOf(key);
      
        if i < 0 then begin
            i := configKeys.add(key);
            configValues.insert(i, val);
            exit;
        end else begin
            configValues[i] := val;
        end;
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    var
        i, j, spacePos, curIndex: integer;
        curLine, testStr, key, val: string;

        fileLines : TStringList;
    begin
        configKeys := TStringList.create;
        configValues := TStringList.create;
        
        fileLines := TStringList.Create;
        fileLines.loadFromFile(configFile);
        
        for i:=0 to fileLines.count-1 do begin
            curLine := trim(fileLines[i]);
            if (length(curLine) > 0) then begin
            
                if (curLine[1] <> '#') then begin 
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
                        if (key <> '') then begin
                            SetConfig(key, val);
                        end;
                    end else begin
                        key := curLine;
                        val := '';
                        SetConfig(key, val);
                    end;
                end;
            end;
        end;
    
        Result := 0;
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
    
    function getObjectLinksTo(entry: IInterface): IInterface;
    var
        value: IInterface;
    begin
        value := ElementByPath(entry, 'Value\Object Union\Object v2\FormID');
        
        Result := nil;
        
        if(not assigned(value)) then begin
            exit;
        end;
        
        Result := LinksTo(value);
    end;
    
    function BoolToStr(str: string): boolean;
    begin
        Result := (str = 'True');
    end;
    
    function checkPropValue(propType: string; expectedValue: string; prop: IInterface): boolean;
    var
        valueString: string;
        valueBool: boolean;
        valueInt: integer;
        valueFloat: float;
    begin
        
        Result := false;
        valueString := GetElementEditValues(prop, propType);
        
        // AddMessage('Comparing: '+expectedValue+'='+valueString);
        
        if (propType = 'Int32') then begin 
            //valueInt := ParseInt(expectedValue);
            Result := (StrToInt(expectedValue) = StrToInt(valueString));
            exit;
        end;

        if (propType = 'Float') then begin 
            // maybe add a comparison function?
            Result := (StrToFloat(expectedValue) = StrToFloat(valueString));
            exit;
        end;
        
        if (propType = 'Bool') then begin 
            Result := (BoolToStr(expectedValue) = BoolToStr(valueString));
            exit;
        end;
        
        Result := (expectedValue = valueString);
        
    end;
    
    function checkObjectEmpty(prop: IInterface): boolean;
    var
        valueRecord : IInterface;
    begin
        Result := false;
        valueRecord := getObjectLinksTo(prop);
        if(not assigned(valueRecord)) then begin
            // this means: the property for this object is set, but it should be empty
            Result := true;
        end;
    end;
    
    function checkDefaultEmptyValue(propType: string; prop: IInterface): boolean;
    var
        expectedString: string;
    begin
        expectedString := '';
        Result := false;
        
        if(propType = 'Object') then begin
            Result := checkObjectEmpty(prop);
            exit;
        end;
        
        if (propType = 'Int32') or (propType = 'Float') then begin 
            Result := checkPropValue(propType, '0', prop);
            exit;
        end;
        
        if (propType = 'Bool') then begin 
            Result := checkPropValue(propType, 'False', prop);
            exit;
        end;
        
        Result := checkPropValue(propType, '', prop);
        
    end;
    
    function processScalarEntry(path: string; nameKey: string; entry: IInterface): boolean;
    var
        propType: string;
        name: string;
        valueString: string;
        valueRecord: IInterface;
        
        configIndex: integer;
        expectedValue: string;
    begin
        Result := false;
    
        name := GetElementEditValues(entry, nameKey);
        propType := GetElementEditValues(entry, 'Type');
        
        // AddMessage('SCALAR: Path: '+path+', Name: '+name);
        

        
        // important part
        configIndex := configKeys.indexOf(path+'.'+nameKey);
        // AddMessage('Got key index: '+IntToStr(configIndex));
        if(configIndex < 0) then begin
            // no explicit mention exists, but maybe the parent exists?
            configIndex := configKeys.indexOf(path);
            if(configIndex < 0) then begin
                // nope. disregard.
                exit;
            end;
            
            Result := checkDefaultEmptyValue(propType, entry);
            exit;            
        end;

        // here, we might have a value
        // for objects, check defaultemptyvalue right away
        if(propType = 'Object') then begin
            Result := checkDefaultEmptyValue(propType, entry);
            exit; 
        end;
        // for everything else, check if we have an explicit default value
        expectedValue := configValues[configIndex];
        if(expectedValue = '') then begin
            { // this is somewhat shitty because of the way I made it, but well... }
            { if(propType = 'String') then begin }
                { Result := checkPropValue(propType, entry); }
                { exit; }
            { end; }
            
            // otherwise, default
            Result := checkDefaultEmptyValue(propType, entry);
            exit; 
        end;
        
        // finally, compare the value from the config
        Result := checkPropValue(propType, expectedValue, entry);
        
        // valueString := GetElementEditValues(entry, propType);

        // AddMessage('Scalar; '+nameKey+' ('+propType+') = '+valueString);
    
        
        
        
    end;
    
    procedure processStruct(path: string; struct: IInterface);
    var
        i: integer;
        curMember, val: IInterface;
        isEmpty: boolean;
        memberName: string;
    begin
        val := struct;//ElementByName(struct, 'Value\Array of Struct');

        //for i := 0 to ElementCount(val)-1 do begin
        i := 0;
        while (i < ElementCount(val)) do begin
            curMember := ElementByIndex(val, i);

            isEmpty := processScalarEntry(path, 'memberName', curMember);
            if(isEmpty) then begin
                // try erasing it
                memberName := GetElementEditValues(curMember, 'memberName');

                AddMessage('ERASING: '+path+'['+IntToStr(i)+'].'+memberName);
                RemoveElement(val, curMember);

            end else begin
                i := i + 1;
            end;
        end;
    end;
    
    function processArray(path: string; arr: IInterface): boolean;
    var
        propType, arrayType: string;
        val, curMember: IInterface;
        i, arrayLength: integer;
    begin 
        Result := false;
        val := ElementByName(arr, 'Value');
        if(not assigned(val)) then begin
            exit;
        end;
        
       
    
        propType :=GetElementEditValues(arr, 'Type');
        arrayType := copy(propType, 10, 99);
        
        // AddMessage('This is an array of: '+arrayType);
        
        val := ElementByName(val, propType);
        
        arrayLength := ElementCount(val);

        if(arrayLength = 0) then begin
            Result := true;
            exit;
        end;
        
        for i := 0 to arrayLength-1 do begin
            curMember := ElementByIndex(val, i);
            // dumpElem(val);
            
            if(arrayType = 'Struct') then begin
                processStruct(path, curMember); // don't consider the array be part of the path
            end;
        end;
        
        
    end;
    
    function processProperty(path: string; prop: IInterface): boolean;
    var
        propName, propType, subPath: string;
        structRoot: IInterface;
        isEmpty: boolean;
    begin
        Result := false;
        propName := GetElementEditValues(prop, 'propertyName');
        propType := GetElementEditValues(prop, 'Type');
        
        subPath := path+'.'+propName;

        if(startsWith('Array of', propType)) then begin
            Result := processArray(subPath, prop);
        end else if(propType = 'Struct') then begin 
            structRoot := ElementByPath(prop, 'Value\Struct');
            processStruct(subPath, structRoot);
        end else begin
            Result := processScalarEntry(path, 'propertyName', prop);
            
        end;        
    end;
    
    
    procedure processScript(script: IInterface);
    var
        properties, prop: IInterface;
        i: integer;
        scriptName : string;
    begin
        scriptName := GetElementEditValues(script, 'scriptName');
        // AddMessage('script: '+scriptName);
        properties := ElementByName(script, 'Properties');
        
        // for i := 0 to ElementCount(properties)-1 do begin
        i := 0;
        while(i<ElementCount(properties)) do begin
            prop := ElementByIndex(properties, i);
            //dumpElem(prop);
            if(processProperty(scriptName, prop)) then begin
                
                AddMessage('ERASING: '+scriptName+'.'+GetElementEditValues(prop, 'propertyName'));
                RemoveElement(properties, prop);
            
            end else begin
                i := i+1;
            end;
        end;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        scriptRoot, scripts, curScript: IInterface;
        i: integer;
    begin
        Result := 0;
        
        scriptRoot := ElementByName(e, 'VMAD - Virtual Machine Adapter');
        
        if(not assigned(scriptRoot)) then begin
            exit;
        end;
        AddMessage('Checking '+EditorID(e));
        
        scripts := ElementByName(scriptRoot, 'Scripts');
        
        for i := 0 to ElementCount(scripts)-1 do begin
            curScript := ElementByIndex(scripts, i);
               
            BeginUpdate(curScript);
            try 
                processScript(curScript);
            finally
                EndUpdate(curScript);
            end;
            
        end;

        // comment this out if you don't want those messages

        // processing code goes here

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
    end;

end.