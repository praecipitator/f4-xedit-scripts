{
    Helper functions to compile scripts
}
unit praCompiler;

    var 
        PapyrusCompiler: string;
        DefaultFlagsFile: string;
        ScriptIncludes: TStringList;
    
    procedure initCompiler();
    begin
        // this is a config of sorts.
        
        ScriptIncludes := TStringList.create;
        // Put in here the include directories for scripts.
        // They will be passed to the compiler with the -i flag in the order specified here.
        // This configuration works for me, but it might not work for you.
        ScriptIncludes.add('..\Script-Includes\SCRIPTS\Source\user');//I:\Steam\steamapps\common\Fallout 4\Script-Includes\SCRIPTS\Source\user
        ScriptIncludes.add('..\Script-Includes\SCRIPTS');
        ScriptIncludes.add('SCRIPTS\Source\Base');
        ScriptIncludes.add('SCRIPTS\Source\user');
        ScriptIncludes.add('SCRIPTS');
        
        // these are default values. Probably don't change them
        PapyrusCompiler := DataPath + '..\Papyrus Compiler\PapyrusCompiler.exe';
        DefaultFlagsFile := DataPath + 'SCRIPTS\source\Base\Institute_Papyrus_Flags.flg';
    end;

    
    function assembleCompilerParamsForVanilla(inputFile: string; outputDir: string): string;
    var
        i: integer;
        includes: String;
    begin
        Result := '"'+inputFile + '" ';
        //
        includes := '';
        for i:=0 to ScriptIncludes.count-1 do begin
            if(i>0) then begin
                includes := includes + ';';
            end;
            includes := includes + DataPath+ScriptIncludes[i];
        end;
        
        Result := Result + ' -i="'+includes+'"';
        
        Result := Result + ' -f="'+DefaultFlagsFile+'" -o="'+outputDir+'"';
        
        Result := Result + ' -op -r';
    end;
    
    {
        Attempts to compile the given inputFile.
        outputDir is the directory where the resulting pex should end up.
    }
    function doCompile(inputFile: string; outputDir: string): boolean;
    var
        exePath, paramStr: string;
        resultCode: LongWord;
    begin
        
        Result := true;


        exePath := PapyrusCompiler;
        paramStr := assembleCompilerParamsForVanilla(inputFile, DataPath+'SCRIPTS');


        
        // try to mkdir
        if(not DirectoryExists(outputDir)) then begin
            if(not ForceDirectories(outputDir)) then begin
                AddMessage('Could not create '+outputDir);
                Result := false;
                exit;
            end;
        end;
        
        AddMessage('Command: '+exePath+' '+paramStr);
        
    
        resultCode := ShellExecuteWait(
            TForm(frmMain).Handle,                  // parent window handle, use 0 for none
            'open',                                 // verb
            exePath,                            // application
            paramStr,                           // parameters
            '',                                     // working directory
            0                           // SW_SHOWNORMAL = window mode
        );
        
        //AddMessage('code '+inttostr(resultCode));
        if(resultCode <> 0) then begin 
            AddMessage('Error compiling '+inputFile);
            Result := false;
        end else begin 
            AddMessage('Compiling successful!');
        end;
        
        
    end;
    
    function compileScriptByName(name: string): boolean;
    var
        sourcePath, outputDir, resultPex: string;
    begin
        sourcePath := scriptNameToSourcePath(name);
        resultPex := scriptNameToPexPath(name);
        outputDir := scriptNameToOutputDir(name);
        
        if(not fileExists(sourcePath)) then begin
            AddMessage('Source '+sourcePath+' not found!');
            AddMessage('Nothing to compile!');
            result := false;
            exit;
        end;
        
        Result := doCompile(sourcePath, outputDir);
        
        if(not FileExists(resultPex)) then begin
            AddMessage('Pex file not generated! '+resultPex);
            Result := false;
        end;
        
    end;
    
    {
        returns the substring of str up to the first occurence of separator
    }
    function getStringUpTo(str: string; separator: string): string;
    var
        i: integer;
        char: string;
    begin
        Result := str;
        for i := 1 to length(str) do begin
            char := copy(str, i, 1);
            
            if(char = separator) then begin
                Result := copy(str, 1, i-1);
                exit;
            end;
        end;
    end;
    
    {
        returns the substring of str up to the last occurence of separator
    }
    function getStringUpToLast(str: string; separator: string): string;
    var
        i: integer;
        char: string;
    begin
        Result := str;
        for i := length(str) downto 1 do begin
            char := copy(str, i, 1);
            
            if(char = separator) then begin
                Result := copy(str, 1, i-1);
                exit;
            end;
        end;
    end;

    {
        etracts the directory in which the given file is
    }
    function getFileDir(curPath: string): string;
    begin
        Result := getStringUpToLast(curPath, '\');
    end;
    
    {
        extracts the first namespace of a script name
    }
    function getScriptNamespace(scriptName: string): string;
    begin
        Result := getStringUpTo(scriptName, ':');
    end;
    
    {
        converts a script name to the path of the corresponding PSC file
    }
    function scriptNameToSourcePath(name: string): string;
    begin
        Result := StringReplace(name, ':', '\', [rfReplaceAll]);
        Result := DataPath + 'SCRIPTS\Source\user\' + Result + '.psc';
    end;
    
    {
        converts a script name to the path of the corresponding PEX file
    }
    function scriptNameToPexPath(name: string): string;
    begin
        Result := StringReplace(name, ':', '\', [rfReplaceAll]);
        Result := DataPath + 'SCRIPTS\' + Result + '.pex';
    end;
    
    {
        for a script name, gets the directory where the corresponding PEX file should be put
    }
    function scriptNameToOutputDir(name: string): string;
    begin
        Result := StringReplace(name, ':', '\', [rfReplaceAll]);
        
        // strip off the last part
        
        Result := getFileDir(DataPath + 'SCRIPTS\' + Result);
    end;

    {
        Makes sure the directory where this file exists. 
    }
    function ensureDirectory(filename: string): boolean;
    var
        dirPath: string;
    begin
        Result := true;
        dirPath := getFileDir(filename);
        if(not DirectoryExists(dirPath)) then begin
            Result := ForceDirectories(dirPath);
        end;
    end;
end.