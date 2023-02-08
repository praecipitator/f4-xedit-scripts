{
    Checks edids of objects you run it on for uniqueness and that the length is <= 99
}
unit userscript;
	var
		edids: TStringList;
    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
		edids := TStringList.create();
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
	var
		edid: string;
    begin
        Result := 0;

        // comment this out if you don't want those messages
		edid := EditorID(e);
        if(edid = '') then begin
            exit;
        end;

		if(edids.indexOf(edid) >= 0) then begin
			AddMessage('DUPLICATE: ' + FullPath(e));
		end;
		
		edids.add(edid);

		if(length(edid) > 99) then begin
			AddMessage('FOUND: '+IntToStr(length(edid)) + FullPath(e));
		end;
        // processing code goes here
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
		edids.free();
    end;

end.