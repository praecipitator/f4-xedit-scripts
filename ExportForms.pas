{
    Run on statics or formlists, then select a text file.
    It will add the EditorIDs of all encountered statics to the file, if they aren't in there already.
}
unit ExportForms;
    uses praUtil;
    
    var
        curList: TStringList;
        curPath: string;
    
    function CreateSaveFileDialogNoOverwrite(title: string; filter: string = ''; initialDir: string = ''): TSaveDialog;
    var
        objFile: TSaveDialog;
    begin
        objFile := TSaveDialog.Create(nil);
        Result := nil;

        objFile.Title := title;
        // objFile.Options := objFile.Options + [ofOverwritePrompt];

        if(filter <> '') then begin
            objFile.Filter := filter;
            objFile.FilterIndex := 1;
        end;
        Result := objFile;
    end;


    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    var
        objFile: TSaveDialog;
    begin
        Result := 0;
        
        objFile := CreateSaveFileDialogNoOverwrite('File of EditorIDs', 'Text files|*.txt|All Files|*.*', '');
        
        try
            if objFile.Execute then begin
                curPath := objFile.FileName;
            end else begin
                Result := 1;
                exit;
            end;
        finally
            objFile.free;
        end;
       
        curList := TStringList.create;
        
        if(FileExists(curPath)) then begin
            curList.loadFromFile(curPath);
        end;
       
        // AddMessage(path);
    end;
    
    procedure maybeAdd(edid: string);
    begin
        if(curList.indexOf(edid) = -1) then begin
            curList.add(edid);
        end;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        curSig: string;
        i: integer;
        curEntry: IInterface;
    begin
        Result := 0;
        
        curSig := Signature(e);
        
        if(curSig = 'STAT') then begin
            maybeAdd(EditorID(e));
            exit;
        end;
        
        if(curSig = 'FLST') then begin
            for i:=0 to getFormListLength(e)-1 do begin
                curEntry := getFormListEntry(e, i);
                if(signature(curEntry) = 'STAT') then begin
                    maybeAdd(EditorID(curEntry));
                end;
            end;           
        end;

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
        
        curList.saveToFile(curPath);
    end;

end.