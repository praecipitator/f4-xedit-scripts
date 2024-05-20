{
    Ensures the EditorIDs of the forms it's run on are <= 87 characters.
    87, because the CK might still add DUPLICATE000 to the EditorID.
}
unit userscript;
    const
        maxEdidLength = 87; // 99-12, because 12 is the length of DUPLICATE000
        
    {
        Calculates a string's CRC32
        To output as string, use IntToHex(foo, 8)

        Function by zilav
    }
    function StringCRC32(s: string): Cardinal;
    var
        ms: TMemoryStream;
        bw: TBinaryWriter;
        br: TBinaryReader;
    begin
        ms := TMemoryStream.Create;
        bw := TBinaryWriter.Create(ms);
        bw.Write(s);
        bw.Free;
        ms.Position := 0;
        br := TBinaryReader.Create(ms);
        Result := wbCRC32Data(br.ReadBytes(ms.Size));
        br.Free;
        ms.Free;
    end;

    function shortenWithCrc32(input: string): string;
    var
        part: string;
    begin
        if(length(input) > maxEdidLength) then begin
            part := copy(input, 1, maxEdidLength-9);
            Result := part + '_' + IntToHex(StringCRC32(input), 8);
            exit;
        end;
        Result := input;
    end;

    function fixEditorID(form: IInterface): boolean;
    var
        curEdid, newEdid: string;
    begin
        Result := false;
        curEdid := EditorID(form);
        newEdid := shortenWithCrc32(curEdid);
        if(curEdid <> newEdid) then begin
            SetElementEditValues(form, 'EDID', newEdid);
            Result := true;
        end;
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
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


		if(fixEditorID(e)) then begin
			AddMessage('FOUND: '+IntToStr(length(edid)) + ' -> '+EditorID(e));
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