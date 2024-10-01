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
        DEBUGFOO: boolean;
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

    function doShittyRounding(x: float; numDigits: cardinal): float;
    var
        helper: float;
        delta, tester, haveDigits, digitsLog: integer;
    begin
        if(x = 0) then begin
            Result := 0;
            exit;
        end;

        if(x < 0) then begin
            Result := doShittyRounding(x * -1, numDigits) * -1;
            exit;
        end;

        digitsLog := Trunc(Log10(x));

        if(digitsLog >= 0) then begin
            haveDigits := digitsLog + 1;
        end else begin
            haveDigits := (digitsLog) - 1;
        end;
        delta := numDigits-haveDigits;
        helper :=  Power(10.0, delta);
        tester := Round(x * helper);
        Result := tester / helper;
    end;

    function formatFloatForNif(f: float): string;
    begin
        Result := FormatFloat('0.000000', f);
    end;
    function areFloatsCloseEnough(x, y: float): boolean;
    var
        signX, signY: boolean;
        percent: float;
    begin
        if(x = y) then begin
            Result := true;
            exit;
        end;

        if(floatEquals(x, y)) then begin
            Result := true;
            exit;
        end;

        signX := (x > 0);
        signY := (y > 0);

        if(signX <> signY) then begin
            Result := false;
            exit;
        end;

        if(not signX) then begin
            x := x * -1;
            y := y * -1;
        end;

        if(x > y) then begin
            percent := y/x;
        end else begin
            percent := x/y;
        end;

        // AddMessage('percent = '+FloatToStr(percent));

        Result := (1 - percent) <= 0.1; // no clue what a good tolerance to check is
    end;

    procedure fixCoords(backupCoords: string; vertex: TwbNifBlock);
    var
        i, n: integer;
        explodeHelperOld, explodeHelperNew: TStringList;
        curCoordOld, curCoordNew, temp: float;
    begin
        explodeHelperOld := TStringList.create();
        explodeHelperOld.Delimiter := ' ';
        explodeHelperOld.StrictDelimiter := TRUE;
        explodeHelperOld.DelimitedText := backupCoords;

        explodeHelperNew := TStringList.create();
        explodeHelperNew.Delimiter := ' ';
        explodeHelperNew.StrictDelimiter := TRUE;
        //explodeHelperNew.DelimitedText := vertex;

        for i:=0 to 2 do begin
            explodeHelperNew.DelimitedText := vertex.EditValues['Vertex'];
            // go over the coords
            curCoordOld := StrToFloat(explodeHelperOld[i]);
            curCoordNew := StrToFloat(explodeHelperNew[i]);

            temp := curCoordOld;
            n := 0;
            // now what?
            if(not areFloatsCloseEnough(curCoordOld, curCoordNew)) then begin
                AddMessage('Found aberrant numbers at '+IntToStr(i)+'! '+FloatToStr(curCoordOld)+' vs '+FloatToStr(curCoordNew));
                while(not areFloatsCloseEnough(curCoordOld, curCoordNew)) do begin
                    temp := temp + 0.0001; // maybe this should be a percentual thing somehow

                    explodeHelperNew[i] := FloatToStr(temp);

                    vertex.EditValues['Vertex'] := explodeHelperNew.DelimitedText;
                    // and read it back
                    explodeHelperNew.DelimitedText := vertex.EditValues['Vertex'];
                    curCoordNew := StrToFloat(explodeHelperNew[i]);

                    n := n + 1;
                end;
                AddMessage('Took '+IntToStr(n)+' iterations to fix');
{
}
            end;

        end;

        explodeHelperNew.free();
        explodeHelperOld.free();
    end;

    procedure postProcessVertexData(vertexDataNif: TwbNifBlock; vertexDataStr: string);
    var
        vertexDataJson, curEntry, vertexArray: TJsonObject;
        vertexArrayJson: TJsonArray;
        i, j: integer;
        curVertex, curNumber: string;
        explodeHelper: TStringList;
        x, y, z: float;
        vertexArrayNif: TwbNifBlock;
        testval: cardinal;
    begin
        vertexDataJson := TJsonObject.Parse(vertexDataStr);

        vertexArrayJson := vertexDataJson.A['Vertex Data'];
        vertexArrayNif  := vertexDataNif;

        if(vertexArrayNif = nil) then begin
            AddMessage('vertexArrayNif FAIL');
        end;


        if(vertexArrayNif.count <> vertexArrayJson.count) then begin
            AddMessage('ERRROR: ARRAY LENGTH MISMATCH');
            exit;
        end;

        for i:=0 to vertexArrayJson.count-1 do begin
            curEntry := vertexArrayJson.O[i];
            curVertex := curEntry.S['Vertex'];
            fixCoords(curVertex, vertexArrayNif[i]);
            {
            // 2168
            explodeHelper := TStringList.create();
            explodeHelper.Delimiter := ' ';
            explodeHelper.StrictDelimiter := TRUE;
            explodeHelper.DelimitedText := curVertex;
            // now explodeHelper should have 3 entries for X, Y, Z
            x := StrToFloat(explodeHelper[0]);
            y := StrToFloat(explodeHelper[1]);
            z := StrToFloat(explodeHelper[2]);



            vertexArrayNif[i].EditValues['Vertex'] := formatFloatForNif(doShittyRounding(x, 6))+' '+formatFloatForNif(doShittyRounding(y, 6))+' '+formatFloatForNif(doShittyRounding(z, 6));
            if(i = 2168) then begin
                y := y + 0.0001; // this seems to be the smallest value I can add without it breaking
                vertexArrayNif[i].EditValues['Vertex'] := formatFloatForNif(doShittyRounding(x, 6))+' '+formatFloatForNif(doShittyRounding(y, 6))+' '+formatFloatForNif(doShittyRounding(z, 6));
                AddMessage('Y explodehelper: '+explodeHelper[1]+', as float: '+FloatToStr(y)+', shitty: '+formatFloatForNif(doShittyRounding(y, 6)));
                AddMessage('Reverse: '+vertexArrayNif[i].EditValues['Vertex']);
            end;

            // writeHalfPrecVertexCoordsManually(x, y, z, vertexArrayNif[i]);

            explodeHelper.free();
            }
        end;

        // vertexArrayJson.SaveToFile('F:\SteamSSD\steamapps\common\Fallout 4\xEdit\foobar.json');

        vertexArrayJson.free();
    end;

    procedure ProcessNif(nifPath: string);
    var
        d, i, numTriShapes, test: integer;
        rootBlock, vertexDesc, vertexData: TwbNifBlock;
        Nif: TwbNifFile;
        vertexBackup: string;
        vertexFlags: cardinal;
        canContinue: boolean;
    begin


        Nif := TwbNifFile.Create;
        try
            //Nif.LoadFromFile(nifPath);
            d := dfFloatDecimalDigits;
            dfFloatDecimalDigits := 16;

            Nif.LoadFromFile(nifPath);
            // AddMessage('Processing file '+nifPath);
            numTriShapes := 0;

            test := Nif.BlocksCount;
            for i:=0 to test-1 do begin
                rootBlock := Nif.Blocks[i];

                if(triShapeTypes.indexOf(rootBlock.BlockType) >= 0) then begin
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
                            // vertexBackup := RoundVertexData(vertexBackup);

                            vertexData.FromJson(vertexBackup);
                            postProcessVertexData(vertexData, vertexBackup);


                            // exit;
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

                // canContinue := false; // DEBUG!!!

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
        dfFloatDecimalDigits := d;
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
