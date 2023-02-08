{
    Applies material swaps from the base object to placed references. Run this on a cell.
    See https://forums.nexusmods.com/index.php?/topic/6992141-custom-material-swapping-problem/
}
unit ApplyMatSwap;

    function Process(e: IInterface): integer;
    var
        baseForm, baseMatSwap, refMatSwap: IInterface;
    begin
        Result := 0;
        
        if (Signature(e) <> 'REFR') then begin
            exit;
        end;
        
        baseForm := LinksTo(ElementBySignature(e, 'NAME'));
        
        if (Signature(baseForm) <> 'STAT') and (Signature(baseForm) <> 'SCOL') then begin
            exit
        end;
        
        baseMatSwap := LinksTo(ElementByPath(baseForm, 'Model\MODS'));
        refMatSwap := LinksTo(ElementByPath(e, 'XMSP'));
        
        if ((assigned(baseMatSwap)) and (not assigned(refMatSwap))) then begin
            AddMessage('Doing '+FullPath(e));
            SetElementEditValues(e, 'XMSP', IntToHex(GetLoadOrderFormID(baseMatSwap), 8));
        end;

    end;

end.