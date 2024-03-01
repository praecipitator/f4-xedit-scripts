{
    Scans all the files you run this on, and generates a CSV containing all the Addon Nodes it found.
    You will be asked where to save the CSV.
}
unit ListAllAddonNodes;
    var
        outData: TStringList;
        targetFile: string;

    function saveFileAs(title: string): string;
    var
        objFile: TSaveDialog;
    begin
        objFile := TSaveDialog.Create(nil);
        Result := '';

        objFile.Title := title;
        objFile.Options := objFile.Options + [ofOverwritePrompt];


        objFile.Filter := 'CSV files|*.csv|All files|*.*';
        objFile.FilterIndex := 1;

        //Result := objFile;

        try
            if objFile.Execute then begin
                Result := objFile.FileName;
            end;
        finally
            objFile.free;
        end;
    end;


    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    var
        ext: string;
    begin
        outData := TStringList.create;
        outData.add('Filename,Node Index,Editor ID');
        
        targetFile := saveFileAs('Save CSV as');
        if(targetFile = '') then begin
            AddMessage('Cancelled');
            Result := 1;
            exit;
        end;
        
        ext := LowerCase(ExtractFileExt(targetFile));
        if(ext = '') then begin
            targetFile := targetFile + '.csv';
        end;

        AddMessage('Target file: '+targetFile);
        Result := 0;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        curIndex, numSoFar: integer;
    begin
        Result := 0;

        if(Signature(e) <> 'ADDN') or (not IsMaster(e)) then begin
            exit;
        end;

        curIndex := GetElementNativeValues(e, 'DATA');

        outData.add(GetFileName(GetFile(e))+','+IntToStr(curIndex)+','+EditorID(e));
        numSoFar := (outData.count - 1);
        if((numSoFar mod 50) = 0) then begin
            AddMessage('Found '+IntToStr(numSoFar)+' AddOn Nodes so far');
        end;
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        AddMessage('Saving file '+targetFile);
        outData.saveToFile(targetFile);
        Result := 0;
        outData.free();
    end;

end.