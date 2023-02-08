{
    don't quite remember what this did TBH
}
unit userscript;
    uses praUtil;
    
    var minDate: cardinal;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        minDate := 11537;
    end;
    
    
    procedure outputCell(cell: IInterface; hasVis: boolean; hasPrecomb: boolean);
    begin
        if(hasVis and hasPrecomb) then exit;
        
        AddMessage(DisplayName(cell)+' PREVIS: '+BoolToStr(hasVis)+'; PRECOMB: '+BoolToStr(hasPreComb));
    end;
    
    function decodeHex(hex:string): cardinal;
    var
        regex: TPerlRegEx;
        byte1, byte2: cardinal;
    begin
        Result := 0;
        regex := TPerlRegEx.Create();

        try
            regex.RegEx := '([0-9A-F]+) ([0-9A-F]+)';
            regex.Subject := hex;

            if(regex.Match()) then begin
                byte1 := IntToStr('$' + regex.Groups[1]);
                byte2 := IntToStr('$' + regex.Groups[2]);
                // swap bytes, it seems to be the nr of days since (2000-08-00) minus one
                //AddMessage(regex.Groups[1]+'a b'+regex.Groups[2]);
                Result := (byte2 shl 8) + byte1;
                //AddMessage(IntToHex(Result, 8));
            end;
        finally
            RegEx.Free;
        end;
        
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        rvis: IInterface;
        hasVis: boolean;
        hasPreComb: boolean;
        visDate, precombDate: cardinal;
    begin
        Result := 0;

        if(Signature(e) <> 'CELL') then begin
            exit;
        end;
{
VISI=24 1F -> 24 1F
PCMB=11 2D

}
        visDate := decodeHex(GetElementEditValues(e, 'VISI'));
        precombDate := decodeHex(GetElementEditValues(e, 'PCMB'));
        rvis := pathLinksTo(e, 'RVIS');
        
        hasPreComb := (precombDate >= minDate);
        hasVis := ((visDate >= minDate) and (assigned(rvis)));

        outputCell(e, hasVis, hasPreComb);
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
    end;

end.