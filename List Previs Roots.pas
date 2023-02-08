{
    Lists the PreVis roots of the cells it is run on
}
unit userscript;
    uses praUtil;
    var
        list: TStringList;
    
    procedure outputCell(cell: IInterface);
    begin
        
        AddMessage(IntToHex(FormID(cell), 8)+' '+DisplayName(cell));
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        list := TStringList.create();
        Result := 0;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        rvis: IInterface;
        str: string;
    begin
        Result := 0;

        // comment this out if you don't want those messages
        if(Signature(e) <> 'CELL') then exit;

        
        rvis := pathLinksTo(e, 'RVIS');
        if(assigned(rvis)) then begin
            str := IntToHex(FormID(rvis), 8);
            if(list.indexOf(str) < 0) then begin
                list.addObject(str, rvis);
            end;
        end;
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        i: integer;
        kw: IInterface;
    begin
        for i:=0 to list.count-1 do begin
            kw := ObjectToElement(list.Objects[i]);
            outputCell(kw);
        end;
        Result := 0;
        
        list.free();
    end;

end.