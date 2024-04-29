{
    Batch-remove Full Prec from nif files. Run on anything, you will be asked to select a nif or a directory.
    If "Create Backups" is checked, the backups will be created in the same directory as the original, with ".bak" appended.
    If a backup cannot be created, the file will not be overwritten.
    
    So far, only BSTriShape, BSMeshLODTriShape, and BSTriShapeSubIndex are processed. If there are more, add them to triShapeTypes in Initialize()
}
unit NifBatchDeFullPrec;

    uses praUtil;

    const
        FLAG_FULL_PREC = 16384;
        configFile = ScriptsPath + 'NIF - Batch De-FullPrec-ifier.cfg';

    var
        numFiles: integer;
        lastPath: string;
        processingMode: integer; // 0 => single file, 1 => directory, 2 => directory recursive
        makeBackup: boolean;
        triShapeTypes: TStringList;
    //============================================================================
    procedure loadConfig();
    var
        i, j, breakPos: integer;
        curLine, curKey, curVal: string;
        lines : TStringList;
    begin
        // default
        lastPath := wbDataPath;
        makeBackup := true;
        processingMode := 0;

        if(not FileExists(configFile)) then begin
            exit;
        end;
        lines := TStringList.create;
        lines.LoadFromFile(configFile);

        for i:=0 to lines.count-1 do begin
            curLine := lines[i];
            breakPos := -1;

            for j:=1 to length(curLine) do begin
                if(curLine[j] = '=') then begin
                    breakPos := j;
                    break;
                end;
            end;

            if breakPos <> -1 then begin
                curKey := trim(copy(curLine, 0, breakPos-1));
                curVal := trim(copy(curLine, breakPos+1, length(curLine)));

                if(curKey = 'lastPath') then begin
                    // maybe validate this?
                    lastPath := curVal;
                end else if(curKey = 'makeBackup') then begin
                    makeBackup := StrToBool(curVal);
                end else if(curKey = 'processingMode') then begin
                    processingMode := StrToInt(curVal);
                    if(processingMode < 0) then begin
                        processingMode := 0;
                    end else if(processingMode > 2) then begin
                        processingMode := 2;
                    end;
                    
                end;
            end;
        end;

        lines.free();
    end;
    //============================================================================
    procedure saveConfig();
    var
        lines : TStringList;
    begin
        lines := TStringList.create;


        lines.add('lastPath='+lastPath);
        lines.add('makeBackup='+BoolToStr(makeBackup));
        lines.add('processingMode='+IntToStr(processingMode));

        lines.saveToFile(configFile);
        lines.free();
    end;
    //============================================================================
    function ShellCopy(src, dst: string): boolean;
    begin
        // mostly stolen from mteFunctions... Main change is the usage of ShellExecuteWait (to make it synchronous) and checking existence of dst to return whenever it worked
        ShellExecuteWait(TForm(frmMain).Handle, 'open', 'cmd', '/C copy /Y "'+src+'" "'+dst+'"', ExtractFilePath(src), SW_HIDE);
        Result := FileExists(dst);
    end;
    //============================================================================
    function createBackup(filePath: string): boolean;
    var
        backupPath: string;
    begin
        backupPath := filePath + '.bak';
        if(FileExists(backupPath)) then begin
            // try deleting
            if(not DeleteFile(backupPath)) then begin
                Result := false;
                exit;
            end;
        end;

        if(not ShellCopy(filePath, backupPath)) then begin
            Result := false;
            exit;
        end;

        Result := true;
    end;
    //============================================================================
    function stripSlash(path: string): string;
    begin
        Result := path;

        if(SameText(copy(path, length(path), 1), '\')) then begin
            Result := copy(path, 0, length(path)-1);
        end;
    end;
    //============================================================================
    procedure ProcessDirectory(dir: string; recursive: boolean);
    var
        searchResult : TSearchRec;
        curFile, dirStripped: string;
    begin
        dirStripped := stripSlash(dir);

        if FindFirst(dirStripped+'\*', faAnyFile, searchResult) = 0 then begin
            repeat
                // ignore . and ..
                if(searchResult.Name <> '.') and (searchResult.Name <> '..') then begin
                    curFile := dirStripped+'\'+searchResult.Name;

                    if((searchResult.attr and faDirectory) = faDirectory) then begin
                        // dir
                        if(recursive) then begin
                            ProcessDirectory(curFile, recursive);
                        end;
                    end else begin
                        // file
                        if(strEndsWithCI(curFile, '.nif')) then begin
                            ProcessNif(curFile);
                        end;
                    end;
                end;
            until FindNext(searchResult) <> 0;

            // Must free up resources used by these successful finds
            FindClose(searchResult);
        end;
    end;

    //============================================================================

    procedure ProcessNif(nifPath: string);
    var
        i, numTriShapes, test: integer;
        rootBlock, vertexDesc, vertexData: TwbNifBlock;
        Nif: TwbNifFile;
        vertexBackup: string;
        vertexFlags: cardinal;
        canContinue: boolean;
    begin


        Nif := TwbNifFile.Create;
        try
            //Nif.LoadFromFile(nifPath);
            // d := dfFloatDecimalDigits;
            // dfFloatDecimalDigits := 16;
            //dfFloatDecimalDigits := 6; // do I need this?

            Nif.LoadFromFile(nifPath);
            // AddMessage('Processing file '+nifPath);
            numTriShapes := 0;

            test := Nif.BlocksCount;
            for i:=0 to test-1 do begin
                rootBlock := Nif.Blocks[i];

                if(triShapeTypes.indexOf(rootBlock.BlockType) >= 0) then begin
                    // AddMessage('checking '+IntToSTr(i)+' '+rootBlock.EditValues['Name']+' '+rootBlock.name+' '+rootBlock.BlockType );

                    // AddMessage('Trying to get the stuff '+rootBlock.EditValues['Vertex Desc']); // nope
                    // foo := rootBlock.Elements['Vertex Desc']; nope
                    // AddMessage('Trying to get the stuff '+rootBlock.VertexDesc); // nope
                    // AddMessage('Trying to get the stuff '+rootBlock.EditValues['VertexDesc']);  // nope
                    // AddMessage('Trying to get the stuff '+rootBlock.Elements['VertexDesc']); // type mismatch!
                    // AddMessage('Trying to get the stuff '+foo.EditValues['VF']); // string 'VF_VERTEX | VF_UV | VF_NORMAL | VF_TANGENT | VF_FULLPREC', this sux
                    // AddMessage('Trying to get the stuff '+IntToStr(vertexDesc.NativeValues['VF'])); // YES!!! 16816 vs 432 0 16384

                    vertexDesc := rootBlock.Elements['VertexDesc'];
                    vertexFlags := vertexDesc.NativeValues['VF'];
                    if (vertexFlags and FLAG_FULL_PREC <> 0) then begin
                        numTriShapes := numTriShapes + 1;
                        vertexFlags := vertexFlags and (not FLAG_FULL_PREC);

                        vertexData := rootBlock.Elements['Vertex Data'];
                        if(vertexData <> nil) then begin
                            // super hack: backup the vertices as json string
                            vertexBackup := vertexData.toJson(vertexData);
                            vertexDesc.NativeValues['VF'] := vertexFlags;
                            // restore backup
                            vertexData.FromJson(vertexBackup);
                        end else begin
                            AddMessage('WARN: looks like '+nifPath+' has no vertex data for block '+IntToStr(i));
                            vertexDesc.NativeValues['VF'] := vertexFlags;
                        end;
                    end;
                end;


                // if(Nif.Blocks[i].EditValues['Name'] = nodeName) then begin
                //    Result := Nif.Blocks[i];
                //    exit;
                // end;
            end;

            if(numTriShapes > 0) then begin
                numFiles := numFiles + 1;
                // AddMessage('File '+nifPath+' modified');
                AddMessage('Processed file '+nifPath+' with '+IntToStr(numTriShapes)+' TriShapes');

                canContinue := true;
                if(makeBackup) then begin
                    if (not createBackup(nifPath)) then begin
                        canContinue := false;
                        AddMessage(' -> Failed to create backup! Will NOT overwrite the file!');
                    end;
                end;

                if(canContinue) then begin
                    Nif.SaveToFile(nifPath);
                    AddMessage(' -> File saved.');
                end;
            // end else begin
            //    AddMessage(' -> Nothing to do.');

            end;
        finally
            Nif.free();
        end;
        //dfFloatDecimalDigits := d;
    end;

    //============================================================================
    procedure btnSrcClick(Sender: TObject);
    var
        edSrc: TLabeledEdit;
        cmbMode: TComboBox;
        s: string;
    begin
        edSrc := TLabeledEdit(TForm(Sender.Parent).FindComponent('edSrc'));
        cmbMode := TComboBox(TForm(Sender.Parent).FindComponent('cmbMode'));

        if(cmbMode.ItemIndex = 0) then begin
            // normal dialog
            s := ShowOpenFileDialog('Select a NIF file', 'Nif Files|*.nif');
        end else begin
            s := SelectDirectory('Select a directory', '', edSrc.Text, nil);// I would have used sdShowEdit and sdNewUI, but they are not implemented. nil seems to be the only valid value here.
        end;

        if s <> '' then begin
            edSrc.Text := s;
        end;
    end;



    //============================================================================
    function Initialize: Integer;
    var
        frm: TForm;
        edSrc: TLabeledEdit;
        btnOk, btnCancel, btnSrc: TButton;
        cbCreateBackups: TCheckBox;
        lbl: TLabel;
        cmbMode: TComboBox;
        pnl: TPanel;
        i, xOffset, yOffset: integer;
    begin
        loadConfig();
        
        numFiles := 0;

        triShapeTypes := TStringList.create();
        triShapeTypes.CaseSensitive := false;

        // I think there are more, TODO add them
        triShapeTypes.add('BSTriShape');
        triShapeTypes.add('BSMeshLODTriShape');
        triShapeTypes.add('BSTriShapeSubIndex');

        frm := TForm.Create(nil);
        try
            frm.Caption := 'Nif De-Full-Precision-ifier';
            frm.Width := 500;
            frm.Height := 220;
            frm.Position := poMainFormCenter;
            frm.BorderStyle := bsDialog;

            xOffset := 12;
            yOffset := 8;
            //////////////

            lbl := TLabel.Create(frm); lbl.Parent := frm;
            lbl.Left := xOffset;
            lbl.Top := yOffset;
            lbl.Caption := 'Processing mode';

            cmbMode := TComboBox.Create(frm);
            cmbMode.Parent := frm;
            cmbMode.Name := 'cmbMode';
            cmbMode.Left := xOffset;
            cmbMode.Top := yOffset + 18;
            cmbMode.Width := 300;
            cmbMode.Style := csDropDownList;
            cmbMode.DropDownCount := 3;

            cmbMode.Items.Add('One single file');
            cmbMode.Items.Add('All files in directory');
            cmbMode.Items.Add('All files in directory and subdirectories');
            cmbMode.ItemIndex := processingMode;

            //////////////
            yOffset := yOffset + 66;
             ///////////
            edSrc := TLabeledEdit.Create(frm); // wait, TLabeledEdit is implemented?
            edSrc.Parent := frm;
            edSrc.Name := 'edSrc';
            edSrc.Left := xOffset;
            edSrc.Top := yOffset;
            edSrc.Width := frm.Width - 70;
            edSrc.LabelPosition := lpAbove;
            edSrc.EditLabel.Caption := 'Source path';
            edSrc.Text := lastPath;

            btnSrc := TButton.Create(frm);
            btnSrc.Parent := frm;
            btnSrc.Top := edSrc.Top - 1;
            btnSrc.Left := edSrc.Left + edSrc.Width + 6;
            btnSrc.Width := 32;
            btnSrc.Height := 22;
            btnSrc.Caption := '...';
            btnSrc.OnClick := btnSrcClick;
            //////////////
            yOffset := yOffset + 28;

            cbCreateBackups := CreateCheckbox(frm, xOffset, yOffset, 'Create backups');
            cbCreateBackups.checked := makeBackup;

            btnOk := TButton.Create(frm);
            btnOk.Parent := frm;
            btnOk.Caption := 'OK';
            btnOk.ModalResult := mrOk;
            btnOk.Left := frm.Width - 176;
            btnOk.Top := frm.Height - 62;

            btnCancel := TButton.Create(frm); btnCancel.Parent := frm;
            btnCancel.Caption := 'Cancel';
            btnCancel.ModalResult := mrCancel;
            btnCancel.Left := btnOk.Left + btnOk.Width + 8;
            btnCancel.Top := btnOk.Top;

            pnl := TPanel.Create(frm); pnl.Parent := frm; pnl.Left := 8; pnl.Top := btnOk.Top - 12; pnl.Width := frm.Width - 20; pnl.Height := 2;

            if frm.ShowModal = mrOk then begin
                if edSrc.Text <> '' then begin

                    makeBackup := cbCreateBackups.checked;
                    processingMode := cmbMode.ItemIndex;
                    lastPath := edSrc.Text;

                    saveConfig();

                    if(cmbMode.ItemIndex = 0) then begin
                        if(FileExists(edSrc.Text )) then begin
                            AddMessage('Processing single nif '+edSrc.Text);
                            ProcessNif( edSrc.Text );
                        end else begin
                            AddMessage('Error: '+ edSrc.Text +' doesn''t exist!');
                        end;
                    end else begin
                        if(DirectoryExists(edSrc.Text)) then begin
                            AddMessage('Processing directory '+edSrc.Text+', recursive: '+BoolToStr(cmbMode.ItemIndex = 2));
                            ProcessDirectory(edSrc.Text, cmbMode.ItemIndex = 2);
                        end else begin
                            AddMessage('Error: '+ edSrc.Text +' doesn''t exist!');
                        end;
                    end;
                    // ProcessNif( 'F:\MO2-Games\Fallout4\mods\Sim Settlements 2 - Pra''s Random Addon 2 DEV\Meshes\TEMP\subwayBedDouble.nif');// edSrc.Text );
                end;
            end;
        finally
            frm.Free;
        end;

        Result := 0;
    end;

    function Finalize: integer;
    begin
        Result := 0;
        triShapeTypes.free();
        AddMessage(IntToStr(numFiles)+' files processed.');
    end;

end.
