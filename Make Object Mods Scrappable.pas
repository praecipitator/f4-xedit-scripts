{
    Run on OMODs
}
unit MakeModsScrappable;

    uses praUtil;

    var
        componentBlacklist: TStringList;
        componentFactor: float;
        targetFile: IInterface;

    //function fixedHighestOverrideOrSelf

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;

        componentBlacklist := TStringList.create;
        componentFactor := 0.5;

        componentBlacklist.add('c_Antiseptic');
        componentBlacklist.add('c_Adhesive');
        componentBlacklist.add('c_Acid');
        componentBlacklist.add('c_Oil');
    end;

    function findCobj(res: IInterface): IInterface;
    var
        i: integer;
        cur, fnam, bnam, fvpa, flstResult: IInterface;
        sig: string;
    begin
        for i:=0 to ReferencedByCount(res)-1 do begin
            cur := HighestOverrideOrSelf(ReferencedByIndex(res, i), 9000);
            sig := Signature(cur);
            if(sig = 'COBJ') then begin
                // seems that BNAM and FNAM must be empty
                // FNAM might be the scrap KW
                bnam := ElementByPath(cur, 'BNAM');
                fnam := ElementByPath(cur, 'FNAM');
                if(assigned(bnam) or assigned(fnam)) then begin
                    continue;
                end;

                fvpa := ElementByPath(cur, 'FVPA');
                if(assigned(fvpa)) then begin
                    Result := cur;
                    exit;
                end;
            end else if(sig = 'FLST') then begin
                flstResult := findCobj(cur);
                if(assigned(flstResult)) then begin
                    Result := flstResult;
                    exit;
                end;
            end;
        end;
    end;

    procedure addCompToJson(ingredObj: IInterface; ingredCount: integer; res: TJsonObject);
    var
        subEntry: TJsonObject;
        edid: string;
    begin
        if(ingredCount <= 0) then exit;
        edid := EditorID(ingredObj);

        subEntry := res.O[edid];
        if(subEntry.I['count'] > 0) then begin
            subEntry.I['count'] := subEntry.I['count'] + ingredCount;
        end else begin
            subEntry.I['count'] := ingredCount;
            subEntry.I['formID'] := FormID(ingredObj);
            res.O[edid] := subEntry;
        end;
    end;

    procedure getAllMiscComponents(misc: IInterface; numMiscs: integer; res: TJsonObject);
    var
        cvpa, curCvpa, ingredObj, maybeMisc: IInterface;
        i, ingredCount, curCount: integer;
    begin
        cvpa := ElementByPath(misc, 'CVPA');

        // hack if someone put a component misc in a cobj
        if(ElementCount(cvpa) = 1) then begin
            curCvpa := ElementByIndex(cvpa, i);
            ingredObj := pathLinksTo(curCvpa, 'Component');
            ingredCount := GetElementEditValues(curCvpa, 'Count');
            if(ingredCount = 1) then begin
                maybeMisc := pathLinksTo(ingredObj, 'MNAM');
                if(isSameForm(maybeMisc, misc)) then begin
                    curCount := Round(componentFactor * numMiscs);
                    addCompToJson(ingredObj, numMiscs, res);
                    exit;
                end;
            end;
        end;


        for i:=0 to ElementCount(cvpa) - 1 do begin
            curCvpa := ElementByIndex(cvpa, i);

            ingredObj := pathLinksTo(curCvpa, 'Component');
            ingredCount := GetElementEditValues(curCvpa, 'Count');

            curCount := Round(ingredCount * componentFactor * numMiscs);
            addCompToJson(ingredObj, curCount, res);
        end;
    end;

    procedure applyComponentsToMisc(oldMisc: IInterface; res: TJsonObject);
    var
        misc: IInterface;
        ingredObj, cvpa, curCvpa: IInterface;
        i, ingredCount: integer;
        edid: string;
    begin
        if(res.count() <= 0) then begin
            exit;
        end;

        misc := getExistingElementOverride(oldMisc, targetFile);
        if(not assigned(misc)) then begin
            addRequiredMastersSilent(oldMisc, targetFile);
            misc := wbCopyElementToFile(oldMisc, targetFile, False, True);
        end;

        AddMessage('Applying components to ' + FullPath(oldMisc));

        // apply to misc
        removeByPath(misc, 'CVPA');
        cvpa := ensurePath(misc, 'CVPA');
        // one exist by default

        for i:=0 to res.count()-1 do begin

            edid := res.names[i];
            ingredObj := getFormByLoadOrderFormID(res.O[edid].I['formID']);
            ingredCount := res.O[edid].I['count'];

            if(i=0) then begin
                curCvpa := ElementByIndex(cvpa, 0);
            end else begin
                curCvpa := ElementAssign(cvpa, HighInteger, nil, False);
            end;

            setPathLinksTo(curCvpa, 'Component', ingredObj);
            setElementEditValues(curCvpa, 'Count', ingredCount);
        end;
    end;

    function getCobjComponents(cobj: IInterface): TJsonObject;
    var
        fvpa, curFvpa, ingredObj: IInterface;
        i, ingredCount, curCount: integer;
        res, subEntry: TJsonObject;
        edid, sig: string;
    begin
        fvpa := ElementByPath(cobj, 'FVPA');

        res := TJsonObject.create();

        for i:=0 to ElementCount(fvpa)-1 do begin
            curFvpa := ElementByIndex(fvpa, i);

            ingredObj := pathLinksTo(curFvpa, 'Component');
            ingredCount := GetElementEditValues(curFvpa, 'Count');
            sig := Signature(ingredObj);
            edid := EditorID(ingredObj);

            if(sig = 'CMPO') then begin
                if(componentBlacklist.indexOf(edid) < 0) then begin
                    curCount := Round(ingredCount * componentFactor);
                    addCompToJson(ingredObj, curCount, res);
                end;
            end else if(sig = 'MISC') then begin
                curCount := Round(ingredCount * componentFactor);
                getAllMiscComponents(ingredObj, curCount, res);
            end;
        end;

        Result := res;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        misc, cobj, winningOr, miscOr: IInterface;
        res: TJsonObject;
    begin
        Result := 0;

        if(Signature(e) <> 'OMOD') then begin
            exit;
        end;

        winningOr := HighestOverrideOrSelf(e, 900);

        //isElementUnsaved

        misc := HighestOverrideOrSelf(pathLinksTo(winningOr, 'LNAM'), 9000);
        if(not assigned(misc)) then exit;

        if(isSameFile(GetFile(misc), targetFile)) then begin
            if(isElementUnsaved(misc)) then exit;
        end;

        cobj := findCobj(winningOr);
        if(not assigned(cobj)) then exit;

        if not Assigned(targetFile) then begin
            targetFile := FileSelect('Select Target File');

            if not Assigned(targetFile) then begin
                AddMessage('File selection dialog was cancelled or failed.');
                Result := 1;
                Exit;
            end;
        end;

        // do stuff
        res := getCobjComponents(cobj);
        applyComponentsToMisc(misc, res);

        res.free();
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;

        componentBlacklist.free();

        if(assigned(targetFile)) then begin
            CleanMasters(targetFile);
        end;
    end;

end.