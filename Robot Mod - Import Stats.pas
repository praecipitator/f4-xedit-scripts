{
    Script to import modifications for robot from a spreadsheet. run on something within targetfile.
    columns must be separated by , only. Lines can be commented out using // or ; in the first cell.
    
}
unit ImportRobotMods;

    uses praUtil, robotModLib;
    
    var
        targetFile: IInterface;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
 
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    begin
        Result := 0;

        // comment this out if you don't want those messages
        // AddMessage('Processing: ' + FullPath(e));
        if(not assigned(targetFile)) then begin
            targetFile := GetFile(e);
            doInit(targetFile);
        end;

    end;
    
    function getNumFromLine(line: TStringList; index: integer): integer;
    var
        resStr: string;
        resTest: integer;
    begin
        Result := 0;
        
        if(line.count >= index) then begin
            resStr := line[index];
            if(resStr <> '') then begin
                resTest := 0;
                try 
                    resTest := StrToInt(resStr);
                    Result := resTest;
                except
                    // meh
                end;
            end;
        end;
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        sourceFile, curLine, edid, desc: string;
        csvLines, csvCols: TStringList;
        i, strength, perception, endurance, intelligence, charisma, agility, luck: integer;
        
        elem: IInterface;
    begin
        Result := 0;
        
        sourceFile := ShowOpenFileDialog('Select Models file', 'CSV Files|*.csv|All Files|*.*');
        
        if(sourceFile = '') then begin
            AddMessage('No source file');
            Result := 1;
            exit;
        end;
        
        csvLines := TStringList.create;
        csvLines.LoadFromFile(sourceFile);
        
        for i:=1 to csvLines.count-1 do begin
            curLine := trim(csvLines.Strings[i]);
            if(curLine = '') then begin
                continue;
            end;
            // legacy
            if(strStartsWith(curLine, ';') or strStartsWith(curLine, '";')) then continue;
            // new
            if(strStartsWith(curLine, '//') or strStartsWith(curLine, '"//')) then continue;
            
            csvCols := TStringList.create;

            csvCols.Delimiter := ',';
            csvCols.StrictDelimiter := TRUE;
            csvCols.DelimitedText := curLine;
            
            if(csvCols.count < 9) then begin
                csvCols.free();
                continue;
            end;
            
            edid := trim(csvCols[0]);
            if(edid = '') then begin
                csvCols.free();
                continue;
            end;
            
            elem := FindObjectByEdid(edid);
            if(not assigned(elem)) then begin
                AddMessage('WARN: Failed to find object '+edid+', skipping');
                csvCols.free();
                continue;
            end;
            
            // elem := getWinningOverrideBefore(elem, targetFile);
            
            strength        := getNumFromLine(csvCols, 2);
            perception      := getNumFromLine(csvCols, 3);
            endurance       := getNumFromLine(csvCols, 4);
            charisma        := getNumFromLine(csvCols, 5);
            intelligence    := getNumFromLine(csvCols, 6);
            agility         := getNumFromLine(csvCols, 7);
            luck            := getNumFromLine(csvCols, 8);
            
            desc := '';
            if(csvCols.count > 9) then begin
                desc := trim(csvCols[9]);
            end;
            
            AddMessage(curLine);
            AddMessage('S+'+IntToStr(strength)+' P+'+IntToStr(perception)+' E+'+IntToStr(endurance)+' C+'+IntToStr(charisma)+' I+'+IntToStr(intelligence)+' A+'+IntToStr(agility)+' L+'+IntToStr(luck)+' desc: '+desc);
            
            setStatsDesc(elem, strength, perception, endurance, charisma, intelligence, agility, luck, desc);
            
            csvCols.free();
        end;
        
        
        
        csvLines.free();
        Result := 0;
    end;

end.