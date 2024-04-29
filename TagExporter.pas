{
    Attempts to export VIS tags from a file into an override .txt
}
unit VISExporter;

    //uses VisOverrides;
    uses praFunctions;


    function Initialize: integer;
    begin
        prepareOverrides();    
    end;

    function Process(e: IInterface): integer;
    var
      i: integer;
      frm: TForm;
      clb: TCheckListBox;
      curSig: String;

      curName: String;
      newElem: IInterface;
      curEdid: String;
      curTag: String;
      
      outputTextFile: String;
    begin
        if Signature(e) = 'TES4' then
            Exit;
            
        outputTextFile := ScriptsPath + 'tagging-overrides\_auto-export.txt';
      
        curName := DisplayName(e);
        if curName <> '' then begin 
        
            curSig := Signature(e);
            if 
                (curSig = 'MISC') or 
                //(curSig = 'OMOD') or 
                (curSig = 'KEYM') or 
                (curSig = 'AMMO') or 
                (curSig = 'ARMO') or 
                (curSig = 'BOOK') or 
                (curSig = 'WEAP') or 
                (curSig = 'ALCH') or 
                (curSig = 'NOTE') then begin 
                
                    curTag := extractVisTag(curName);
                    if curTag <> '' then begin
                        curEdid := GetElementEditValues(e, 'EDID');
                        
                        //AddMessage('Adding '+curTag+' for '+curEdid);
                        registerOverride(curEdid, curTag);
                    end;
            end;
        end;

      
    end;

    //============================================================================
    function Finalize: integer;
    var
        i: integer;
      dlgSave: TSaveDialog;
      slExport: TStringList;
      ExportFileName: string;
    begin
        slExport := TStringList.Create;
        
        for i:=0 to overridesKeys.count-1 do begin
            
            slExport.Add(overridesKeys[i]+'='+overridesVals[i]);
        end;
        
        ExportFileName := 'export.txt';
        // Edit Scripts\vis-overrides\
        

      if slExport.Count <> 0 then begin
        dlgSave := TSaveDialog.Create(nil);
        try
          dlgSave.Options := dlgSave.Options + [ofOverwritePrompt];
          dlgSave.Filter := 'Text (*.txt)|*.txt';
          dlgSave.InitialDir := ScriptsPath+'tagging-overrides';
          dlgSave.FileName := ExportFileName;
          if dlgSave.Execute then begin
            ExportFileName := dlgSave.FileName;
            AddMessage('Saving ' + ExportFileName);
            
            slExport.SaveToFile(ExportFileName);
          end;
        finally
          dlgSave.Free;
        end;
      end;
      slExport.Free;
    end;

end.