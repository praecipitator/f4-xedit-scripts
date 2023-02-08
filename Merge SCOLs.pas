{
    unfinished script to merge two or more SCOLs
}
unit MergeScols;

    uses praUtil;
    uses CobbLibrary;

    var
        scolData: TJsonObject;
        targetFile: IInterface;

    function AddNewRecordToGroup(const g: IInterface; const s: String): IInterface;
    begin
        Result := Add(g, s, True);
        if not Assigned(Result) then
            Result := Add(g, s, True); // tries twice because
    end;

    function createScol(name: string): IInterface;
    var
        scolGroup: IInterface;
    begin
        scolGroup := GroupBySignature(targetFile, 'SCOL');
        if(not assigned(scolGroup)) then begin
            scolGroup := Add(targetFile, 'SCOL', True);
        end;

        Result := AddNewRecordToGroup(scolGroup, 'SCOL - Static Collection');

        SetElementEditValues(Result, 'EDID', name);
    end;
    
    procedure fillScol(scol: IInterface);
    var
        parts, curPartForm, curPart, curPartData, curPlacement: IInterface;
        i, j: integer;
        formIdStr: string;
        curPlacementObj, posMin, posMax: TJsonObject;
        formIdPlacements: TJsonArray;
    begin
        parts := ensurePath(scol, 'Parts');
        
        posMin := newVector(0,0,0);
        posMax := newVector(0,0,0);
        
        for i:=0 to scolData.count - 1 do begin
            formIdStr := scolData.names[i];
            formIdPlacements := scolData.A[formIdStr];
            
            curPartForm := getFormByLoadOrderFormID(StrToInt(formIdStr));
            
            if ( i = 0) then begin
                curPart := ElementByIndex(parts, 0);
            end else begin
                curPart := ElementAssign(parts, HighInteger, nil, False);
            end;
            
            SetPathLinksTo(curPart, 'ONAM - Static', curPartForm);
            curPartData := EnsurePath(curPart, 'DATA - Placements');
            
            for j := 0 to formIdPlacements.count-1 do begin
                curPlacementObj := formIdPlacements.O[j];
                
                if (i=0) and (j=0) then begin
                    curPlacement := ElementByIndex(curPartData, 0);
                end else begin
                    curPlacement := ElementAssign(curPartData, HighInteger, nil, False);
                end;
                

                seev(curPlacement, 'Position\X', curPlacementObj.O['pos'].F['x']);
                seev(curPlacement, 'Position\Y', curPlacementObj.O['pos'].F['y']);
                seev(curPlacement, 'Position\Z', curPlacementObj.O['pos'].F['z']);

                seev(curPlacement, 'Rotation\X', curPlacementObj.O['rot'].F['x']);
                seev(curPlacement, 'Rotation\Y', curPlacementObj.O['rot'].F['y']);
                seev(curPlacement, 'Rotation\Z', curPlacementObj.O['rot'].F['z']);
                seev(curPlacement, 'Scale', curPlacementObj.F['scale']);

            end;

        end;
    end;

    procedure addScolData(scolRef: IInterface);
    var
        scale, partScale: float;
        posVector, rotVector, partPosVector, partPosVectorScaled, partRotVector, curPartPlacementData, rotatedData: TJsonObject;
        scolBase, parts, curPart, curPartBase, placements, curPlacement: IInterface;
        i, j: integer;
        formIdString, scaleString: string;
    begin
        scale := 1;
        
        scaleString := geev(scolRef, 'XSCL');
        if(scaleString <> '') then begin
            scale := StrToFloat(scaleString);
        end;

        posVector := getPositionVector(scolRef, 'DATA');
        rotVector := getRotationVector(scolRef, 'DATA');

        scolBase := pathLinksTo(scolRef, 'NAME');
        if(Signature(scolBase) <> 'SCOL') then begin
            exit;
        end;

        parts := ElementByPath(scolBase, 'Parts');
        for i:=0 to ElementCount(parts)-1 do begin
            curPart := ElementByIndex(parts, i);

            curPartBase := pathLinksTo(curPart, 'ONAM');
            AddMessage('CurPart: '+EditorID(curPartBase));
            formIdString := IntToStr(GetLoadOrderFormID(curPartBase));

            placements := ElementByPath(curPart, 'DATA');
            for j:=0 to ElementCount(placements)-1 do begin
                curPlacement := ElementByIndex(placements, j);

                partPosVector := getPositionVector(curPlacement, '');
                partPosVectorScaled := VectorMultiply(partPosVector, scale);
                partRotVector := getRotationVector(curPlacement, '');

                partScale := 1.0;
                scaleString := geev(curPlacement, 'Scale');
                if(scaleString <> '') then begin
                    partScale := StrToFloat(scaleString);
                end;
                
                // curPartPlacementData
                // rotatedData := MoveObjectRelativeToObject(posVector, rotVector, partPosVectorScaled, partRotVector);
                rotatedData := GetCoordinatesRelativeToBase(posVector, rotVector, partPosVectorScaled, partRotVector);

                curPartPlacementData := scolData.A[formIdString].addObject();
                curPartPlacementData.O['pos'].assign(rotatedData.O['pos']);
                curPartPlacementData.O['rot'].assign(rotatedData.O['rot']);
                curPartPlacementData.F['scale'] := partScale * scale;
                
                partPosVector.free();
                partPosVectorScaled.free();
                partRotVector.free();
                rotatedData.free();
            end;
        end;
        
        posVector.free();
        rotVector.free();

    end;

    // Called before c
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        scolData := TJsonObject.create();
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    begin
        Result := 0;


        targetFile := GetFile(e);
        // comment this out if you don't want those messages
        if(Signature(e) = 'REFR') then begin
            AddMessage('Processing: ' + FullPath(e));
            addScolData(e);
        end;

        // processing code goes here

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        newScol : IInterface;
    begin
        Result := 0;

        // try
        //AddMessage(scolData.ToJson());
        AddMessage('Generating SCOL');
        newScol := createScol('foo');
        fillScol(newScol);
        scolData.free();
    end;

end.