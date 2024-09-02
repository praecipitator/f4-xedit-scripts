{
    Attempts to figure out if overrides of Worldspaces or Cells are safe to mark as partial,
    and, if yes, marks them as partial and deletes some data.

    Loosely based on a script by E (01000101)

    The rules are:
        - Worldspaces:
            - never (apparently it isn't actually safe)
        - Cells:
            - Interiors:
                - Must be an ITM
                - Must not contain navmeshes
                - Must contain the "Persistent" subgroup with at least 1 child
                - Must not contain precombine-breaking overrides
            - Exteriors:
                - Never
            - Worldspace persistent cells:
                - Always
    (Terms like "exists" and "contains" refer to the target plugin only)
}
unit MakePartialForm;

    function FilesEqual(file1, file2: IwbFile): boolean;
    begin
        // Should be faster than comparing the filenames
        Result := (GetLoadOrder(file1) = GetLoadOrder(file2));
    end;

    function getExistingElementOverride(sourceElem: IInterface; targetFile: IwbFile): IInterface;
    var
        masterElem, curOverride: IINterface;
        numOverrides, i: integer;
        targetFileName: string;
    begin
        Result := nil;

        masterElem := MasterOrSelf(sourceElem);
        targetFileName := GetFileName(targetFile);

        // important failsafe
        if(FilesEqual(targetFile,  GetFile(masterElem))) then begin
            Result := sourceElem;
            exit;
        end;

        numOverrides := OverrideCount(masterElem);

        for i:=0 to numOverrides-1 do begin
            curOverride := OverrideByIndex(masterElem, i);

            if (FilesEqual(GetFile(curOverride), targetFile)) then begin
                Result := curOverride;
                exit;
            end;
        end;
    end;

    function elementsDeepEqual(e1, e2: IInterface; prevPath: string): boolean;
    var
        i: integer;
        c1, c2, l1, l2: IInterface;
        n1, n2: string;
    begin
        Result := false;


        n1 := DisplayName(e1);
        n2 := DisplayName(e2);

        if(n1 <> n2) then begin
            //AddMessage('DisplayName: '+n1 +' != '+ n2);
            exit;
        end;
        if(ElementCount(e1) <> ElementCount(e2)) then begin
            //AddMessage('ElementCount: '+n1+': '+ElementCount(e1) +' != '+n2+': ' + ElementCount(e2));
            exit;
        end;

        //AddMessage('prevPath '+prevPath);

        if(prevPath = 'Record Header') then begin
            if(n1 <> 'Signature') and (n1 <> 'Record Flags') then begin
                Result := true;
                exit;
            end;
        end;

        l1 := LinksTo(e1);
        l2 := LinksTo(e2);

        if(assigned(l1) or assigned(l2)) then begin
            if(assigned(l1) and assigned(l2)) then begin
                if(not Equals(MasterOrSelf(l1), MasterOrSelf(l2))) then begin
                    //AddMessage('LinksTo 1: '+n1 +' != '+ n2);

                    exit;
                end;
            end else begin
                //AddMessage('LinksTo 2: '+n1 +' != '+ n2);
                exit;
            end;
        end else begin
            if(GetEditValue(e1) <> GetEditValue(e2)) then begin
                //AddMessage('GetEditValue: '+n1+': '+GetEditValue(e1) +' != '+n2+': '+ GetEditValue(e2));
                exit;
            end;

            for i:=0 to ElementCount(e1)-1 do begin
                c1 := ElementByIndex(e1, i);
                c2 := ElementByIndex(e2, i);


                if(not elementsDeepEqual(c1, c2, n1)) then begin
                    //AddMessage('Not equal at '+n1 +' != '+ n2);
                    exit;
                end;
            end;
        end;

        Result := true;
    end;

    function hasPartialFlag(e: IInterface): boolean;
    begin
         Result := (GetElementNativeValues( e, 'Record Header\Record Flags' ) and $4000) <> 0;
    end;

    procedure addPartialFlag(e: IInterface);
    begin
        SetElementNativeValues( e, 'Record Header\Record Flags', GetElementNativeValues( e, 'Record Header\Record Flags' ) or $4000 );
    end;

    function getPrevOverride(e: IInterface): IInterface;
    var
        i, numOverrides: integer;
        prevOverride, elemMaster, curOverride, elemFile: IInterface;
    begin
        elemMaster := Master(e);
        numOverrides := OverrideCount(elemMaster);
        // only check overrides from masters of current file
        elemFile := GetFile(e);
        Result := nil;
        if(HasMaster(elemFile, GetFileName(GetFile(elemMaster)))) then begin
            Result := elemMaster;
        end;

        Result := elemMaster;
        if(numOverrides <= 1) then begin
            // the given element is the only override anyway
            exit;
        end;

        // prevOverride := elemMaster;

        for i:=0 to numOverrides-1 do begin
            curOverride := OverrideByIndex(elemMaster, i);
            if(hasPartialFlag(curOverride)) then continue;
            if(not HasMaster(elemFile, GetFileName(GetFile(curOverride)))) then continue; // skip non-master override

            if(Equals(curOverride, e)) then begin
                // AddMessage('found '+GetFileName(GetFile(prevOverride)));
                Result := prevOverride;
                exit;
            end;
            prevOverride := curOverride;
        end;

        Result := elemMaster;
    end;

    function isITM(e: IInterface): boolean;
    var
        prevOverride: IInterface;
    begin
        prevOverride := getPrevOverride(e);

        Result := elementsDeepEqual(e, prevOverride, '');
    end;

    function isInterior(cell: IInterface): boolean;
    var
        dataFlags: cardinal;
    begin
        // interior flag: 1
        dataFlags := GetElementNativeValues(cell, 'DATA');
        Result := (dataFlags and 1) <> 0;
    end;

    function scanCellForLandAndNavmesh(cell: IInterface): boolean;
    var
        tempCell, r: IInterface;
        i: integer;
        sig: string;
    begin
        Result := false;

        // 9 is temporary
        tempCell := FindChildGroup(ChildGroup(cell), 9, cell);
        for i := 0 to ElementCount(tempCell)-1 do begin
            r := ElementByIndex(tempCell, i);
            sig := Signature(r);

            if (sig = 'NAVM') or (sig = 'LAND') then begin
                AddMessage(' -> Found a '+sig +' record');
                exit;
            end;
        end;

        Result := true;
    end;

    function scanCellForPrecombreakers(cellPrevOverride, modFile: IInterface): boolean;
    var
        tempCell, xpri, xcri, r, refOverride: IInterface;
        i: integer;
    begin
        Result := false;

        xpri := ElementByPath(cellPrevOverride, 'XPRI');
        xcri := ElementByPath(cellPrevOverride, 'XCRI');
        if((not assigned(xpri)) and (not assigned(xcri) )) then begin
            // disregard precombine-less cells
            Result := true;
            exit;
        end;
            //hasPrecombines

        // 9 is temporary
        tempCell := FindChildGroup(ChildGroup(cellPrevOverride), 9, cellPrevOverride);
        for i := 0 to ElementCount(tempCell)-1 do begin
            r := ElementByIndex(tempCell, i);

            if(HasPrecombinedMesh(r)) then begin
                refOverride := getExistingElementOverride(r, modFile);

                if(assigned(refOverride)) then begin
                    if(elementsDeepEqual(r, refOverride, '')) then begin
                        AddMessage('    WARNING: found ITM ref: '+FullPath(refOverride));
                    end else begin
                        AddMessage(' -> Found precombine breaker: '+FullPath(refOverride));
                        exit;
                    end;
                end;
            end;
        end;

        Result := true;
    end;

    function scanCell(cellPrevOverride, cell: IInterface): boolean;
    begin
        Result := false;
        if(not scanCellForLandAndNavmesh(cell)) then begin
            exit;
        end;

        if(not scanCellForPrecombreakers(cellPrevOverride, GetFile(cell))) then begin
            exit;
        end;

        Result := true;
    end;

    function checkInteriorCell(cell: IInterface): boolean;
    var
        persistentCell, prevCellOverride: IInterface;
    begin
        Result := false;
        // must have persistent subgroup
        // 9 is temporary
        // 8 seems to be persistent
        persistentCell := FindChildGroup(ChildGroup(cell), 8, cell);
        // must exist in the same file. does it also have to have at least one thing in there? probably yes
        if(not assigned(persistentCell)) then begin
            AddMessage(' -> has no persistent subgroup');
            exit;
        end;

        if(ElementCount(persistentCell) <= 0) then begin
            AddMessage(' -> has no persistent refs');
            exit;
        end;

        prevCellOverride := getPrevOverride(cell);

        // must be ITM
        if(not elementsDeepEqual(cell, prevCellOverride, '')) then begin
            AddMessage(' -> is not an ITM');
            exit;
        end;

        if(not scanCell(prevCellOverride, cell)) then begin
            exit;
        end;

        Result := true;
        // cell must not contain NAVM
        // cell must not break precombines
    end;

    function findPersistentCell(worldspace: IInterface): IInterface;
    var
        wrldgrup, block: IInterface;
        i: integer;
        flags: cardinal;
    begin
        Result := nil;
        wrldgrup := ChildGroup(worldspace);

        for i := 0 to ElementCount(wrldgrup)-1 do begin
            block := ElementByIndex(wrldgrup, i);
            if(Signature(block) = 'CELL') then begin
                flags := GetElementNativeValues(block, 'Record Header\Record Flags');
                // persistent = 1024
                if((flags and 1024) <> 0) then begin
                    Result := block;
                    exit;
                end;
            end;
        end;
    end;

    function checkExteriorCell(cell: IInterfaec): boolean;
    var
        flags: cardinal;
        prevCellOverride, parentWs, wsPersistentCell, wspcGroup: IInterface;
    begin
        Result := false;

        // if this is the persistent cell: true
        flags := GetElementNativeValues(cell, 'Record Header\Record Flags');
        // persistent = 1024
        if((flags and 1024) <> 0) then begin
            Result := true;
            exit
        end;

        // if this is NOT the persistent cell:
        // I don't know under which circumstances this is actually safe.
        // So far it seems the answer is "never".
        exit;

        prevCellOverride := getPrevOverride(cell);

        // - parent worldspace's persistent cell must be in current file
        parentWs := LinksTo(ElementByPath(cell, 'Worldspace'));
        // how do I find the persistent cell?
        wsPersistentCell := findPersistentCell(parentWs);
        if(not assigned(wsPersistentCell)) then begin
            AddMessage(' -> parent worldspace has no persistent cell in the plugin');
            exit;
        end;

        // this isn't actually relevant
        {
        wspcGroup := FindChildGroup(ChildGroup(wsPersistentCell), 8, wsPersistentCell);
        if(ElementCount(wspcGroup) < 1) then begin
            AddMessage(' -> persistent cell of parent worldspace is empty');
            exit;
        end;
        }

        // must be ITM
        if(not elementsDeepEqual(cell, prevCellOverride, '')) then begin
            AddMessage(' -> is not an ITM');
            exit;
        end;

        // - cell must not contain LAND
        // - cell must not contain NAVM
        // - must not break precombines
        if(not scanCell(prevCellOverride, cell)) then begin
            exit;
        end;

        Result := true;
    end;

    procedure truncateCell(e: IInterface);
    begin
        RemoveElement( e, 'DATA' );
        RemoveElement( e, 'VISI' );
        RemoveElement( e, 'RVIS' );
        RemoveElement( e, 'PCMB' );
        RemoveElement( e, 'XCLC' );
        RemoveElement( e, 'XCLL' );
        RemoveElement( e, 'CNAM' );
        RemoveElement( e, 'ZNAM' );
        RemoveElement( e, 'TVDT' );
        RemoveElement( e, 'MHDT' );
        RemoveElement( e, 'LTMP' );
        RemoveElement( e, 'XCLW' );
        RemoveElement( e, 'XCLR' );
        RemoveElement( e, 'XLCN' );
        RemoveElement( e, 'XWCN' );
        RemoveElement( e, 'XWCU' );
        RemoveElement( e, 'XCWT' );
        RemoveElement( e, 'XOWN' );
        RemoveElement( e, 'XRNK' );
        RemoveElement( e, 'XILL' );
        RemoveElement( e, 'XILW' );
        RemoveElement( e, 'XWEM' );
        RemoveElement( e, 'XCCM' );
        RemoveElement( e, 'XCAS' );
        RemoveElement( e, 'XEZN' );
        RemoveElement( e, 'XCMO' );
        RemoveElement( e, 'XCIM' );
        RemoveElement( e, 'XGDR' );
        RemoveElement( e, 'XPRI' );
        RemoveElement( e, 'XCRI' );
    end;

    procedure truncateWorldspace(e: IInterface);
    begin
        RemoveElement( e, 'RNAM' );
        RemoveElement( e, 'MHDT' );
        RemoveElement( e, 'FULL' );
        RemoveElement( e, 'WCTR' );
        RemoveElement( e, 'LTMP' );
        RemoveElement( e, 'XEZN' );
        RemoveElement( e, 'XLCN' );
        RemoveElement( e, 'Parent' );
        RemoveElement( e, 'CNAM' );
        RemoveElement( e, 'NAM2' );
        RemoveElement( e, 'NAM3' );
        RemoveElement( e, 'NAM4' );
        RemoveElement( e, 'DNAM' );
        RemoveElement( e, 'ICON' );
        RemoveElement( e, 'Cloud Model' );
        RemoveElement( e, 'MNAM' );
        RemoveElement( e, 'ONAM' );
        RemoveElement( e, 'NAMA' );
        RemoveElement( e, 'DATA' );
        RemoveElement( e, 'Object Bounds' );
        RemoveElement( e, 'ZNAM' );
        RemoveElement( e, 'NNAM' );
        RemoveElement( e, 'XWEM' );
        RemoveElement( e, 'TNAM' );
        RemoveElement( e, 'UNAM' );
        RemoveElement( e, 'World Default Level Data' );
        RemoveElement( e, 'OFST' );
        RemoveElement( e, 'CLSZ' );
    end;

    function checkWorldSpace(worldspace: IInterface): boolean;
    begin
        // must be ITM
        Result := IsITM(worldspace);
        if(not Result) then begin
            AddMessage(' -> not an ITM');
        end;
    end;

    function Process(e: IInterface): integer;
    var
        s, curHash, prevHash: string;
        f: integer;
        prevOverride: IInterface;
    begin
        s := Signature(e);

        if( s <> 'CELL' ) and ( s <> 'WRLD' ) then exit;

        AddMessage('Checking '+FullPath(e));

        if (IsMaster( e )) then begin
            AddMessage(' -> not an override, skipping');
            Exit; // Can't make a master record partial - durr
        end;

        if (hasPartialFlag(e)) then begin
            AddMessage(' -> has partial flag already');
            // done already
            exit;
        end;

        {
        // check stuff
        if(s = 'WRLD') then begin
            if(not checkWorldSpace(e)) then begin
                AddMessage(' => SKIPPING');
                exit;
            end;

            AddMessage(' => PROCESSING');
            addPartialFlag(e);
            truncateWorldspace(e);
            exit;
        end;
        }

        if(s = 'CELL' ) then begin
            if(isInterior(e)) then begin
                if(not checkInteriorCell(e)) then begin
                    AddMessage(' => SKIPPING');
                    exit;
                end;
            end else begin
                if(not checkExteriorCell(e)) then begin
                    AddMessage(' => SKIPPING');
                    exit;
                end;
            end;

            AddMessage(' => PROCESSING');
            addPartialFlag(e);
            truncateCell(e);
        end;
    end;
end.
