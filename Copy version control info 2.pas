{
  Copy Version Control Info inside Record Header between plugins.
  Apply script to destination records, then select the source plugin.
  Rcords are matched by FormIDs.
}
unit CopyVCInfo;

var
  fromPlugin: IInterface;


  
//==================================================================================
function Process(e: IInterface): integer;
var
  frm: TForm;
  clb: TCheckListBox;
  i: integer;
  r: IInterface;
begin
  // plugins selection window for the source plugin to copy from
  if not Assigned(fromPlugin) then begin
    frm := frmFileSelect;
    try
      frm.Caption := 'Select plugin to copy from';
      frm.Width := 420;
      clb := TCheckListBox(frm.FindComponent('CheckListBox1'));

      // add files except the current one
      for i := 0 to Pred(FileCount) do
        if not SameText(GetFileName(e), GetFileName(FileByIndex(i))) then begin
          clb.Items.AddObject(GetFileName(FileByIndex(i)), FileByIndex(i));
          Inc(i);
        end;

      // get the first checked file
      if frm.ShowModal = mrOk then
        for i := 0 to Pred(clb.Items.Count) do
          if clb.Checked[i] then begin
            fromPlugin := ObjectToElement(clb.Items.Objects[i]);
            Break;
          end;

      // if nothing is checked then abort
      if not Assigned(fromPlugin) then begin
        Result := 1;
        Exit;
      end;

    finally
      frm.Free;
    end;
  end;

  // copy VC info
  r := RecordByFormID(fromPlugin, FixedFormID(e), False);
  if Assigned(r) then begin
    SetFormVCS1(e, GetFormVCS1(r));
    SetFormVCS2(e, GetFormVCS2(r));
  end;
end;


end.