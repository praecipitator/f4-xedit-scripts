{
    Copies the RNAM of a dialogue into it's NAM1 - Response text
}
unit userscript;
    var
        ToFile: IInterface;


    function findToFile(e: IInterface): boolean;
    var
        i, tpl: integer;
        frm: TForm;
        clb: TCheckListBox;
        g: IInterface;
        edid: widestring;
    begin
        Result := true;
        if not Assigned(ToFile) then begin
            frm := frmFileSelect;
            try
                frm.Caption := 'Select a plugin';
                clb := TCheckListBox(frm.FindComponent('CheckListBox1'));
                clb.Items.Add('<new file>');
                for i := Pred(FileCount) downto 0 do begin

                    if (GetFileName(e) <> GetFileName(FileByIndex(i))) then begin
                        clb.Items.InsertObject(1, GetFileName(FileByIndex(i)), FileByIndex(i))
                    end else begin
                        Break;
                    end;
                end;
                if frm.ShowModal <> mrOk then begin
                    Result := false;
                    Exit;
                end;
                for i := 0 to Pred(clb.Items.Count) do begin
                    if clb.Checked[i] then begin
                        if i = 0 then ToFile := AddNewFile else
                            ToFile := ObjectToElement(clb.Items.Objects[i]);
                            Break;
                    end;
                end;
            finally
                frm.Free;
            end;
            if not Assigned(ToFile) then begin
                Result := false;
                Exit;
            end;
        end;


    end;

    function Initialize: integer;

    begin
        Result := 0;


    end;

    function Process(e: IInterface): integer;
    var
        promptText, responseText: string;
        responseArray, curResponse, newOverride: IInterface;
        i: integer;

    begin
        Result := 0;

        // only dial
        if(Signature(e) <> 'INFO') then begin
            exit;
        end;
        // comment this out if you don't want those messages
        //AddMessage('Processing: ' + FullPath(e));

        promptText := GetElementEditValues(e, 'RNAM');
        if(promptText = '') then begin
            exit;
        end;

        if(not assigned(ToFile)) then begin
            if(not findToFile(e)) then begin
                Result := 1;
                exit;
            end;
        end;

        AddRequiredElementMasters(e, ToFile, False);
        newOverride := wbCopyElementToFile(e, ToFile, True, True);

        responseArray := ElementByPath(newOverride, 'Responses');
        for i:=0 to ElementCount(responseArray)-1 do begin
            curResponse := ElementByIndex(responseArray, i);
            responseText := GetElementEditValues(curResponse, 'NAM1 - Response Text');
            if(responseText = '') then begin
                AddMessage('Doing '+FullPath(curResponse));
                //AddRequiredElementMasters(curResponse, ToFile, False);
                //newOverride := wbCopyElementToFile(curResponse, ToFile, True, True);

                SetElementEditValues(newOverride, 'NAM1 - Response Text', promptText);
            end;
        end;

    end;
end.