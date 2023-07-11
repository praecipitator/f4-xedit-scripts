{
    Run on a cell
}
unit OffsetCellRefs;
    uses praUtil;

    var
        targetCell, targetFile: IInterface;
        offsetX, offsetY, offsetZ: float;
        
        // stats
        numNavmeshes, numRefs: integer;

        // new bounds
        minX, minY, minZ, maxX, maxY, maxZ: float;
        isFirstRef: boolean;

    function getElementToEdit(e: IInterface): IInterface;
    var
        elemFile: IInterface;
    begin
        elemFile := GetFile(e);
        if(isSameFile(elemFile, targetFile)) then begin
            Result := e;
            exit;
        end;

        // addRequiredMastersSilent(e, targetFile);
        Result := getOrCreateElementOverride(e, targetFile);
    end;

    procedure processNavm(navm: IInterface);
    var
        navmToEdit, vertices, curV: IInterface;
        numVertices, i: integer;
        newX, newY, newZ: float;

    begin
        // hard
        navmToEdit := getElementToEdit(navm);

        vertices := ElementByPath(navmToEdit, 'NVNM - Navmesh Geometry\Vertices');
        if(not assigned(vertices)) then begin
            exit;
        end;

        numNavmeshes := numNavmeshes + 1;

        numVertices := ElementCount(vertices);
        for i := 0 to numVertices - 1 do begin
            curV := ElementByIndex(vertices, i);

            newX := GetElementNativeValues(curV, 'X')+offsetX;
            newY := GetElementNativeValues(curV, 'Y')+offsetY;
            newZ := GetElementNativeValues(curV, 'Z')+offsetZ;
            
            if(isFirstRef) then begin
                minX := newX;
                minY := newY;
                minZ := newZ;

                maxX := newX;
                maxY := newY;
                maxZ := newZ;
                isFirstRef := false;
            end else begin
                if(newX < minX) then minX := newX;
                if(newY < minY) then minY := newY;
                if(newZ < minZ) then minZ := newZ;

                if(newX > maxX) then maxX := newX;
                if(newY > maxY) then maxY := newY;
                if(newZ > maxZ) then maxZ := newZ;
            end;
            
            SetElementNativeValues(curV, 'X', newX);
            SetElementNativeValues(curV, 'Y', newY);
            SetElementNativeValues(curV, 'Z', newZ);
        end;
    end;

    procedure processRef(ref: IInterface);
    var
        refToEdit: IInterface;
        newX, newY, newZ: float;
    begin
        numRefs := numRefs + 1;
        // easy
        refToEdit := getElementToEdit(ref);

        newX := GetElementNativeValues(ref, 'DATA\Position\X')+offsetX;
        newY := GetElementNativeValues(ref, 'DATA\Position\Y')+offsetY;
        newZ := GetElementNativeValues(ref, 'DATA\Position\Z')+offsetZ;

        if(isFirstRef) then begin
            minX := newX;
            minY := newY;
            minZ := newZ;

            maxX := newX;
            maxY := newY;
            maxZ := newZ;
            isFirstRef := false;
        end else begin
            if(newX < minX) then minX := newX;
            if(newY < minY) then minY := newY;
            if(newZ < minZ) then minZ := newZ;

            if(newX > maxX) then maxX := newX;
            if(newY > maxY) then maxY := newY;
            if(newZ > maxZ) then maxZ := newZ;
        end;

        SetElementNativeValues(refToEdit, 'DATA\Position\X', newX);
        SetElementNativeValues(refToEdit, 'DATA\Position\Y', newY);
        SetElementNativeValues(refToEdit, 'DATA\Position\Z', newZ);
    end;

    function showGui(curTargetFile: IInterface): boolean;
    var
        frm: TForm;
        grp: TGroupBox;
        yOffset: integer;
        inputX, inputY, inputZ: TEdit;

        btnOk, btnCancel: TButton;
        resultCode: cardinal;

        targetFileBox: TComboBox;
        newFileName: string;
    begin
        Result := false;
        frm := CreateDialog('Offset Interior Cell Refs', 500, 300);

        //CreateLabel(frm, 10, 10, 'Offset Amounts:');
        grp := CreateGroup(frm, 10, 10, 480, 100, 'Offset Amounts');
        yOffset := 8;
        CreateLabel(grp, 20, 16+yOffset, 'X:');
        inputX := CreateInput(grp, 40, 14+yOffset, '0.0');
        inputX.Width := 400;

        yOffset := yOffset + 24;
        CreateLabel(grp, 20, 16+yOffset, 'Y:');
        inputY := CreateInput(grp, 40, 14+yOffset, '0.0');
        inputY.Width := 400;

        yOffset := yOffset + 24;
        CreateLabel(grp, 20, 16+yOffset, 'Z:');
        inputZ := CreateInput(grp, 40, 14+yOffset, '0.0');
        inputZ.Width := 400;

        yOffset := 140;
        CreateLabel(frm, 40, yOffset+2, 'Target File:');
        targetFileBox := CreateFileSelectDropdown(frm, 120, yOffset, 200, curTargetFile, true);
        yOffset := 180;

        btnOk := CreateButton(frm, 80, yOffset, '    OK    ');
        btnCancel := CreateButton(frm, 280, yOffset, '  Cancel  ');

        btnCancel.ModalResult := mrCancel;
        btnOk.ModalResult := mrOk;

        resultCode := frm.showModal();
        if (resultCode <> mrOk) then begin
            frm.free();
            exit;
        end;

        offsetX := StrToFloat(inputX.text);
        offsetY := StrToFloat(inputY.text);
        offsetZ := StrToFloat(inputZ.text);

        if(targetFileBox.ItemIndex = 0) then begin
            // add new file

            if(not InputQuery('Offset Interior Cell Refs', 'Enter New File Name (with or without extension)', newFileName)) then begin
                frm.free();
                exit;
            end;
            newFileName := trim(newFileName);
            if(newFileName = '') then begin
                frm.free();
                exit;
            end;

            if (not strEndsWithCI(newFileName, '.esp')) and (not strEndsWithCI(newFileName, '.esl')) and (not strEndsWithCI(newFileName, '.esm')) then begin
                newFileName := newFileName+'.esp';
            end;

            targetFile := AddNewFileName(newFileName);
            if(not assigned(targetFile)) then begin
                frm.free();
                exit;
            end;
        end else begin
            newFileName := targetFileBox.Items[targetFileBox.ItemIndex];
            targetFile := FindFile(newFileName);

            if(not assigned(targetFile)) then begin
                AddMessage('ERROR');
                frm.free();
                exit;
            end;
        end;

        frm.free();
        Result := true;
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        isFirstRef := true;
        
        numNavmeshes := 0;
        numRefs := 0;

        // new bounds
        minX := 0;
        minY := 0;
        minZ := 0;
        maxX := 0;
        maxY := 0;
        maxZ := 0;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        parentCell: IInterface;
        curSig: string;
    begin
        Result := 0;

        curSig := Signature(e);

        // comment this out if you don't want those messages
        // AddMessage('Processing: ' + FullPath(e));
        if (not isReferenceSignature(curSig)) and (curSig <> 'NAVM') then exit;

        if (not assigned(targetCell)) then begin

            parentCell := pathLinksTo(e, 'CELL');
            if(not assigned(parentCell)) then exit;
            if(GetElementEditValues(parentCell, 'DATA\Is Interior Cell') <> '1') then exit;

            if(not showGui(GetFile(parentCell))) then begin
                Result := 1;
                exit;
            end;

            targetCell := parentCell;
            AddMessage('Processing cell '+Name(targetCell));
        end;
        // otherwise continue and process this ref
        // failsafe
        parentCell := pathLinksTo(e, 'CELL');
        if(not isSameForm(parentCell, targetCell)) then exit;


        AddMessage('Processing '+Name(e));
        if(curSig <> 'NAVM') then begin
            processRef(e);
        end else begin
            processNavm(e);
        end;
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
        
        AddMessage('Processed '+IntToStr(numRefs)+' references and '+IntToStr(numNavmeshes)+' navmeshes.');
        AddMessage('New Cell Bounds are:');
        AddMessage('Xmin: '+FloatToStr(minX)+'; Xmax: '+FloatToStr(maxX));
        AddMessage('Ymin: '+FloatToStr(minY)+'; Ymax: '+FloatToStr(maxY));
        AddMessage('Zmin: '+FloatToStr(minZ)+'; Zmax: '+FloatToStr(maxZ));
        {numNavmeshes := 0;
        numRefs := 0;

        // new bounds
        minX := 0;
        minY := 0;
        minZ := 0;
        maxX := 0;
        maxY := 0;
        maxZ := 0;}
    end;

end.