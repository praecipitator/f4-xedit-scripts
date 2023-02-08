{
    Updates the data in my SS2 vending machine buttons
    Run on the buttons
}
unit userscript;

    uses praUtil;

    var
        baseButton, targetFile: IInterface;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
    end;

    function findCapsValue(e: IInterface): integer;
    var
        sig, valueStr: string;
    begin
        sig := Signature(e);

        if(sig = 'ALCH') then begin
            valueStr := GetElementEditValues(e, 'ENIT\Value');
        end else if(sig = 'WEAP') then begin
            valueStr := GetElementEditValues(e, 'DNAM\Value');
        end else begin
            valueStr := GetElementEditValues(e, 'DATA\Value');
        end;

        if(valueStr = '') then begin
            AddMessage('Failed to find value for this:');
            dumpElem(e);
        end;
        Result := StrToInt(valueStr);
    end;

    function StripComponents(text: string): string;
    var
        temp, c: string;
        len, i, startPos: integer;
        found: boolean;
    begin
        Result := text;
        
        len := strlen(text);
        
        temp := copy(text, len-2, len);
        
        if(temp <> '}}}') then begin
            exit;
        end;
        
        found := false;
        
        for i:=1 to len-3 do begin
            c := copy(text, i, 3);
            if(c = '{{{') then begin
                startPos := i;
                found := true;
                break;
            end;
        end;
        
        
        Result := copy(text, 1, startPos-1);
    end;

    function StripTag(text: String): string;
    var
        temp, c: String;
        hasFirst, hasLast: boolean;
        i, lastPos: integer;
    begin
        temp := text;
        lastPos := -1;
        Result := temp;

        for i:=1 to length(temp) do begin
            c := temp[i];
            if(i = 1) then begin
                if (c = '[') or (c = '|') or (c = '{') or (c = '(') then begin
                    hasFirst := true;
                end else begin
                    exit;
                end;
            end else begin
                if (c = ']') or (c = '|') (c = '}') or (c = ')') then begin
                    hasLast := true;
                    lastPos := i;
                    break;
                end;
            end;
        end;

        if(not hasFirst) or (not hasLast) then begin
            Result := temp;
            exit;
        end;

        Result := copy(temp, lastPos+2, strlen(temp));
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        newRecord, newRecScript, targetItem, targetItemMaster, targetFileObj: IInterface;
        targetFile, itemName, parentFileName: string;
        targetFormID, zeroFormId: cardinal;
        cost, count: integer;
    begin
        Result := 0;

        if(signature(e) <> 'ACTI') then begin
            exit;
        end;

        newRecScript := getScript(e, 'praSS2:VendorMachineButtonRef');

        if(not assigned(newRecScript)) then begin
            exit;
        end;

        // comment this out if you don't want those messages
        AddMessage('Processing: ' + FullPath(e));

        targetItem := getScriptProp(newRecScript, 'ItemToDispense');
        if(not assigned(targetItem)) then begin
            targetFile := getScriptPropDefault(newRecScript, 'sourcePlugin', '');
            targetFormID := getScriptPropDefault(newRecScript, 'FormID', 0);

            if (targetFile = '') or (targetFormID <= 0) then begin
                AddMessage('Got nothing');
                exit;
            end;

            //targetFormID := FileFormIDtoLoadOrderFormID(targetFormID);

            targetItem := getFormByFilenameAndFormID(targetFile, targetFormID);
            if(not assigned(targetItem)) then begin
                AddMessage('Still found no item '+targetFile+', '+IntToHex(targetFormID, 8));//00024544
                exit;
            end;
        end;
        count := GetScriptPropDefault(newRecScript, 'batchSize', 1);
    
        targetItemMaster := MasterOrSelf(targetItem);
        targetItem := WinningOverride(targetItem);

        itemName := StripTag(GetElementEditValues(targetItem, 'FULL'));
        
        itemName := StripComponents(itemName);
        
        cost := findCapsValue(targetItem) * count;

        if(cost > 0) then begin
            // set prop
            setScriptProp(newRecScript, 'CostValueCaps', cost);
            // set prompt
            SetElementEditValues(e, 'ATTX', IntToStr(cost) + ' Caps');
        end else begin
            // do something
            cost := getScriptPropDefault(newRecScript, 'CostValueCaps', 0);
            if(cost <= 0) then begin
                cost := 1;
                setScriptProp(newRecScript, 'CostValueCaps', cost);
            end;
            
            SetElementEditValues(e, 'ATTX', IntToStr(cost) + ' Caps');
        end;
        // set name
        if(count > 1) then begin
            //SetElementEditValues(e, 'FULL', itemName + ' ('+IntToStr(count)+' Units)');
            SetElementEditValues(e, 'FULL', itemName + ' ('+IntToStr(count)+')');
        end else begin
            SetElementEditValues(e, 'FULL', itemName);
        end;
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
    end;

end.