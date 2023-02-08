{
    shows an input for a keyword's EDID. 
    if the keyword exists, it will be added to the objects the scripts runs on
}
unit userscript;

    uses praUtil;
    
    var
        keyword: IInterface;
        
    function findRecord(edid, sig: string): IInterface;
    var
        i: integer;
        curFile: IwbFile;
        group, elem: IInterface;
    begin
        Result := nil;
        for i := 0 to FileCount-1 do
        begin
            curFile := FileByIndex(i);
            
            group := GroupBySignature(curFile, sig);
            if(assigned(group)) then begin
                elem := MainRecordByEditorID(group, edid);
                if(assigned(elem)) then begin
                    Result := elem;
                    exit;
                end;
            end;
        end;
        
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    var
        edid: string;
    begin
        if(not InputQuery('AddKeyword', 'Input Search keyword EDID', edid)) then begin
            Result := 1;
            exit;
        end;
    
        keyword := findRecord(edid, 'KYWD');
        if(not assigned(keyword)) then begin
            AddMessage('Could not find keyword '+edid);
            Result := 1;
            exit;
        end;
    
        Result := 0;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    begin
        Result := 0;

        if(not hasKeywordByPath(e, keyword, 'KWDA')) then begin
            addKeywordByPath(e, keyword, 'KWDA');
        end;

        // processing code goes here

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
    end;

end.