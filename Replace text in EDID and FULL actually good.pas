{
    Search and replace for the EDID and FULL of the selected forms. 
    You will be asked for the search and replacement strings.
}
unit UserScript;


    var
        ReplaceCount: integer;
        StrSearch :string;
        StrReplace: string;

    function Initialize: integer;
    begin
        ReplaceCount := 0;

        if(not InputQuery('Replace Name Parts', 'Input Search String', StrSearch)) then begin
            Result := 1;
            exit;
        end;

        if(not InputQuery('Replace Name Parts', 'Input Replace String', StrReplace)) then begin
            Result := 1;
            exit;
        end;

        Result := 0;
    end;

    procedure SearchAndReplace(e: IInterface; s1, s2: string);
    var
        s: string;
    begin
        if not Assigned(e) then Exit;

        // remove rfIgnoreCase to be case sensitive
        s := StringReplace(GetEditValue(e), s1, s2, [rfReplaceAll, rfIgnoreCase]);

        if not SameText(s, GetEditValue(e)) then begin
            Inc(ReplaceCount);
            AddMessage('Replacing in ' + FullPath(e));
            SetEditValue(e, s);
        end;

    end;

    function Process(e: IInterface): integer;
    begin
        SearchAndReplace(ElementBySignature(e, 'EDID'), StrSearch, StrReplace);
        SearchAndReplace(ElementBySignature(e, 'FULL'), StrSearch, StrReplace);
    end;

    function Finalize: integer;
    begin
        AddMessage(Format('Replaced %d occurences.', [ReplaceCount]));
    end;

end.
