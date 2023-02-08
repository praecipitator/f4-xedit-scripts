{
  New script template, only shows processed records
  Assigning any nonzero value to Result will terminate script
}
unit ExportWyn;

    uses praFunctions;
    
    var
        formIdName, fileNameName: string;
        whiteList: TStringList;
        blackList: TStringList;
    
    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        formIdName   := 'actorBaseFormId';
        fileNameName := 'sourceFile';
        
        whiteList := TStringList.create;
        blackList := TStringList.create;
    
        Result := 0;
    end;
    
    procedure processProp(e: IInterface; list: TStringList);
    var
        i, j, k: Integer;
        val, curMember, curWat: IInterface;
        memberName, memberValue, formId, fileName: string;
    begin
        // get value?
        val := ElementByNamePath(e, 'Value\Array of Struct');
        
        
        for i := 0 to ElementCount(val)-1 do begin
            curMember := ElementByIndex(val, i);

            for j := 0 to ElementCount(curMember)-1 do begin
                curWat := ElementByIndex(curMember, j);
                //dumpElem(curWat);
                memberName := GetElementEditValues(curWat, 'memberName');
                memberValue := getPropVal(curWat);
                addMessage(memberName+' = '+memberValue);
                if(memberName = formIdName) then begin
                    formId := IntToHex(memberValue, 8);
                end;
                
                if(memberName = fileNameName) then begin
                    fileName := memberValue;
                end;
            end;
            
            list.add(formId+';'+fileName);
            
        end;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        whitelistProp, blacklistProp: IInterface;
    begin
        Result := 0;

        // find the prop
        whitelistProp := getPropertyOfScript(e, 'pra:praRenameQuestScript', 'whitelistNpcs');
        blacklistProp := getPropertyOfScript(e, 'pra:praRenameQuestScript', 'blacklistNpcs');
        AddMessage('==Whitelist==');
        processProp(whitelistProp, whiteList);
        AddMessage('==Blacklist==');
        processProp(blacklistProp, blackList);
        
        { AddMessage('==Blacklist=='); }
        { dumpElem(blacklistProp); }

        // processing code goes here

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
        
        whiteList.SaveToFile(ScriptsPath+'wyn-whitelist.txt');
        blackList.SaveToFile(ScriptsPath+'wyn-blacklist.txt');
    end;

end.
