{
    Lists all base forms used in a cell
    Run on cell
}
unit ListUsedItems;
    uses praUtil;
    
    var
        forms: TStringList;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        forms := TStringList.create;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        baseRec: IInterface;
        objName, baseSig: string;
        oldIndex, curCount: integer;
    begin
        Result := 0;
        if(Signature(e) <> 'REFR') then begin
            exit;
        end;
        // comment this out if you don't want those messages
        //AddMessage('Processing: ' + FullPath(e));

        // processing code goes here
        baseRec := PathLinksTo(e, 'NAME');
        
        baseSig := Signature(baseRec);
        
        if
            (baseSig = 'BOOK') or
            (baseSig = 'AMMO') or
            (baseSig = 'ARMO') or
            (baseSig = 'CONT') or
            (baseSig = 'KEYM') or
            (baseSig = 'LVLI') or
            (baseSig = 'NOTE') or
            (baseSig = 'WEAP') or
            (baseSig = 'MISC') or
            (baseSig = 'ALCH') 
        then begin
            // AddMessage(FullPath(baseRec));
            objName := EditorID(baseRec) + ' "' + DisplayName(baseRec) + '" ['+baseSig+':'+IntToHex(FormID(baseRec), 8)+']';
            oldIndex := forms.IndexOf(objName);
            
            if(oldIndex < 0) then begin
                forms.addObject(objName, 1);
            end else begin
                curCount := Integer(forms.Objects[oldIndex]);
                // AddMessage('curCount '+IntToStr(curCount));
                
                forms.Objects[oldIndex] := (curCount+1);
            end;
        end;
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        i, cnt: integer;
    begin
        Result := 0;
        
        for i := 0 to forms.count-1 do begin
            cnt := Integer(forms.Objects[i]);
            AddMessage(forms[i]+' -> '+IntToStr(cnt));
        end;
        
        forms.free();
    end;

end.