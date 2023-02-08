{
    This script, should, in theory, find objects which break precombines.
}
unit userscript;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
    end;
    
    procedure outputError(e: IInterface);
    var
        curMaster, cell, winner: IInterface;
    begin
        curMaster := MasterOrSelf(e);
        winner := HighestOverrideOrSelf(e, 500);
        cell := LinksTo(ElementByPath(curMaster, 'Cell'));
        AddMessage('--------------------------------');
        AddMessage('!!! FOUND PRECOMBINE BREAKER !!!');
        AddMessage('CELL: '+Name(cell));
        AddMessage('REFR: '+Name(curMaster));
        AddMessage('Culprit: '+GetFileName(GetFile(winner)));
        AddMessage('--------------------------------');
    end;
    
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
            // AddMessage('checking='+GetFileName(GetFile(curOverride)));

            if (FilesEqual(GetFile(curOverride), targetFile)) then begin
                // AddMessage('should work');
                Result := curOverride;
                exit;
            end;
        end;
    end;
    
    
    
    function winnerHasPrecombines(e: IInterface): boolean;
    var
        winner, origCell, winCell: IInterface;
        origPrevis, origPrecomb, winPrevis, winPrecomb: string;
    begin
        winner := HighestOverrideOrSelf(e, 500);
        origCell := LinksTo(ElementByPath(e, 'Cell'));

        
        //winCell := HighestOverrideOrSelf(origCell, 500);//LinksTo(ElementByPath(winner, 'Cell'));
        //winCell := LinksTo(ElementByPath(winner, 'Cell'));
        winCell := getExistingElementOverride(origCell, GetFile(winner));
        //AddMessage('A'+Name(winCell));
        
        origPrevis := GetElementEditValues(origCell, 'VISI');
        origPrecomb:= GetElementEditValues(origCell, 'PCMB');
        
        winPrevis := GetElementEditValues(winCell, 'VISI');
        winPrecomb:= GetElementEditValues(winCell, 'PCMB');

        //AddMessage(origPrevis+' '+origPrecomb+' = '+winPrevis+' '+winPrecomb);
        
        Result := (origPrevis<winPrevis) and (origPrecomb<winPrecomb);
        
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        baseElem: IInterface;
        curSig: string;
    begin
        Result := 0;

        // comment this out if you don't want those messages
        if(Signature(e) <> 'REFR') then begin
            exit;
        end;
        
        baseElem := MasterOrSelf(e);

        if(HasPrecombinedMesh(baseElem)) then begin
            if(not IsWinningOverride(baseElem)) then begin
                if(not winnerHasPrecombines(baseElem)) then begin
                    outputError(baseElem);
                end;
            end;
        end;
        //targetForm := LinksTo(ElementByPath(e, 'NAME'));
        //curSig := Signature(targetForm);
        //AddMessage(curSig);
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
    end;

end.