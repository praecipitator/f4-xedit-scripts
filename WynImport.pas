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
        
        whiteList.LoadFromFile(ScriptsPath+'wyn-whitelist.txt');
        blackList.LoadFromFile(ScriptsPath+'wyn-blacklist.txt');
    
        Result := 0;
    end;
    
    procedure processProp(e: IInterface; list: TStringList);
    var
        i, j, k, charpos: Integer;
        val, arrayOfStruct, curStruct, curMember: IInterface;
        memberName, memberValue, formId, fileName, curLine: string;
    begin
        // get value?
        val := ElementByName(e, 'Value');
        arrayOfStruct := ElementByName(val, 'Array of Struct');

        if(assigned(arrayOfStruct)) then begin
            remove(arrayOfStruct);
        end;
        
        SetToDefault(val);
        arrayOfStruct := ElementByName(val, 'Array of Struct');
        {
        // now try adding them back in
        arrayOfStruct := Add(val, 'Array of Struct', False);
        if(not assigned(arrayOfStruct)) then begin
            addMessage('Nope');
        end;}
        
        //curMember :=  ElementAssign(curStruct, HighInteger, nil, False);
        
        for i:=0 to list.count-1 do begin            
            // try adding a struct?
            curLine := list[i];
            charpos := getCharPos(curLine, ';');
            if(charpos > -1) then begin
                
                formId := IntToStr(StrToInt('$'+copy(curLine, 1, charpos-1)));
                fileName := copy(curLine, charpos+1, length(curLine));
                addMessage('id: '+formId+'; name: '+fileName);
                
                curStruct :=  ElementAssign(arrayOfStruct, HighInteger, nil, False);
                addStructMemberScalar(curStruct, fileNameName, 'String', fileName);
                addStructMemberScalar(curStruct, formIdName, 'Int32', formId);
                
            end;
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

    end;

end.
