{
    Tries to use nconvert.exe to check the textures in the target file's Textures.ba2 for correct sizes
}
unit userscript;

    const
        nconvPath = DataPath + '..\Tools\NConvert\nconvert.exe';

    var
        targetFile: IInterface;
        numErrors: integer;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        numErrors := 0;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    begin
        Result := 0;

        // comment this out if you don't want those messages
        targetFile := GetFile(e);

        // processing code goes here

    end;

    function checkDim(x: integer): boolean;
    begin
        if(x = 0) then begin
            Result := false;
            exit;
        end;

        Result := (x and (x - 1)) = 0;
    end;

    procedure checkTextureDims(texPath: string; w, h: integer);
    begin
        if(not checkDim(w)) or (not checkDim(h)) then begin
            numErrors := numErrors+1;
            AddMessage('ERROR! '+texPath+' has invalid dimensions! '+IntToStr(w)+'x'+IntToStr(h));
        end else begin
            // AddMessage('OK: '+texPath+'  '+IntToStr(w)+'x'+IntToStr(h));
        end;
    end;

    function ExtractFileBasename(filename: string): string;
    var
        curExt: string;
    begin
        curExt := ExtractFileExt(filename);

        Result := copy(filename, 0, length(filename)-length(curExt));
    end;

    function checkTextureUsingExternalTool(src, texPath: string): boolean;
    var
        tmpFile, outPath: string;
        proc: TProcess;
        procResult: TStringList;
        i: integer;
        res: cardinal;
        curLine, firstPart, lastPart: string;
        foundW, foundH, p: integer;
    begin
        foundW := -1;
        foundH := -1;
        Result := false;
        if(not FileExists(nconvPath)) then begin
            exit;
        end;
        // extract...
        tmpFile := ProgramPath+'tmp.dds';
        outPath := ProgramPath+'OUTPUT.txt';
        if(FileExists(tmpFile)) then begin
            DeleteFile(tmpFile);
        end;
        
        if(FileExists(outPath)) then begin
            DeleteFile(outPath);
        end;

        ResourceCopy(src, texPath, tmpFile);

        // now try using nconv
        

        res := ShellExecuteWait(
            TForm(frmMain).Handle,                  // parent window handle, use 0 for none
            'open',                                 // verb
            'cmd.exe',                              // application
            // found via trial&error...
            '"/C" "'+nconvPath+'" "-info" "'+tmpFile+'" > "'+outPath+'"', // parameters
            '',                                     // working directory
            SW_HIDE                           // window mode
        );
        
        if(res <> 0) or (not FileExists(outPath)) then begin
            AddMessage('failed to use nconv');
            exit;
        end;
        procResult := TStringList.create;
        
        procResult.loadFromFile(outPath);
        
        Result := true;
        
        for i:=0 to procResult.count-1 do begin
            curLine := procResult[i];
            p := Pos(':', curLine);
            if (p > 0) then begin
                firstPart := trim(Copy(curLine, 1, p-1));
                lastPart  := trim(Copy(curLine, p+1, length(curLine)-p));
                
                if(firstPart = 'Width') then begin
                    foundW := StrToInt(lastPart);
                end else if(firstPart = 'Height') then begin
                    foundH := StrToInt(lastPart);
                end;
            end;
        end;

        // do we have them?
        if(foundW > 0) and (foundH > 0) then begin
            Result := true;
            checkTextureDims(texPath, foundW, foundH);
        end;

        procResult.free();
    end;

    procedure checkTexture(src, texPath: string);
    var
        texStream: TBytesStream;
        bmp: TBitmap;
    begin
        texStream := ResourceOpenData(src, texPath);
        bmp := TBitmap.create;

        if(wbDDSDataToBitmap(texStream, bmp)) then begin
            //AddMessage(IntToStr(bmp.height)+'x'+IntToStr(bmp.width));
            checkTextureDims(texPath, bmp.width, bmp.height);

        end else begin
            if(not checkTextureUsingExternalTool(src, texPath)) then begin
                AddMessage('WARNING! Failed to process '+texPath+': seems we can''t parse that');
            end;
        end;

        bmp.free();
        //texStream.free();
    end;


    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        textures: TStringList;
        i: integer;
        modFileName, ba2name: string;
    begin
        modFileName := ExtractFileBasename(GetFileName(targetFile));


        ba2name := modFileName + ' - Textures.ba2';
        AddMessage('checking '+ba2name);
        Result := 0;
        textures := TStringList.create();


        ResourceList(DataPath+ba2name, textures);

        for i:=0 to textures.count-1 do begin
            //AddMessage(textures[i]);
            checkTexture(DataPath+ba2name, textures[i]);
        end;
        
        AddMessage('Finished. Found '+IntToStr(numErrors)+' issues.');

        textures.free();
    end;

end.