{
    Puts fragments into a namespace. 
    Changes the records, copies the source files, and recompiles them.
    
    See praCompiler.pas for compiler options.
}
unit AddNamespaceToFragments;

    //uses praFunctions;
    uses praCompiler;

    var
        SelectedNamespace: string;
        
    function scriptNameToSourcePath(name: string): string;
    begin
        Result := StringReplace(name, ':', '\', [rfReplaceAll]);
        Result := DataPath + 'SCRIPTS\Source\user\' + Result + '.psc';
    end;
    
    function scriptNameToOutputDir(name: string): string;
    begin
        Result := StringReplace(name, ':', '\', [rfReplaceAll]);
        
        // strip off the last part
        
        Result := getFileDir(DataPath + 'SCRIPTS\' + Result);
    end;    
    
    function createRenamedScriptCopy(oldFileName: string; newFileName: string; oldScriptName: string; newScriptName: string): boolean;
    var
        fileContents: TStringList;
        i: integer;
        curLine, tmp: string;
    begin 
        Result := false;
        fileContents := TStringList.create;
        
        fileContents.loadFromFile(oldFileName);
        
        for i:=0 to fileContents.count-1 do begin
            curLine := fileContents[i];
            tmp := copy(curLine, 1, 10);
            // Addmessage('cur tmp '+tmp);
            if(LowerCase(tmp) = 'scriptname') then begin
                curLine := StringReplace(curLine, oldScriptName, newScriptName, [rfReplaceAll]);
                fileContents[i] := curLine;
                break;
            end;
            // Scriptname Fragments:Terminals:TERM_praSim_plotToolsSettler_02000815 Extends Terminal Hidden Const
            
        end;
        
        if (not ensureDirectory(newFileName)) then begin
            exit;
        end;
        fileContents.saveToFile(newFileName);
        
        Result := true;
    end;
    
    function migrateScript(oldScriptName: string): string;
    var
        newScriptName: string;
        oldScriptSourcePath: string;
        newScriptSourcePath: string;
        outputDir: string;
        resultPexPath: string;
    begin
        Result := '';
        if(LowerCase(getScriptNamespace(oldScriptName)) <> 'fragments') then begin
            AddMessage('Skipping '+oldScriptName);
            exit;
        end;
        newScriptName := SelectedNamespace+':'+oldScriptName;
    
        oldScriptSourcePath := scriptNameToSourcePath(oldScriptName);
        
        if(not FileExists(oldScriptSourcePath)) then begin
            AddMessage('File not found:'+oldScriptSourcePath);
            exit;
        end;
        
        newScriptSourcePath := scriptNameToSourcePath(newScriptName);
        resultPexPath := scriptNameToPexPath(newScriptName);
        
        outputDir := scriptNameToOutputDir(newScriptName);
        
        if(not createRenamedScriptCopy(oldScriptSourcePath, newScriptSourcePath, oldScriptName, newScriptName)) then begin
            AddMessage('Could not create file '+newScriptSourcePath);
            exit;
        end;
        
        // now compile
        if(doCompile(newScriptSourcePath, outputDir)) then begin
            if(fileexists(resultPexPath)) then begin
                // AddMessage('Should have generated '+resultPexPath);
                Result := newScriptName;
            end else begin
                AddMessage('Failed to generate the PEX file: '+resultPexPath);
            end;
        end;
    end;
    
    procedure dumpElement(e: IInterface; prefix: String);
    var
        i: Integer;
        child: IInterface;
    begin
        for i := 0 to ElementCount(e)-1 do begin
            child := ElementByIndex(e, i);
            AddMessage(prefix+DisplayName(child)+'='+GetEditValue(child));
            dumpElement(child, prefix+'  ');
        end
    end;

    
    procedure processFragments(e: IInterface);
    var
        vmad, frags, curFrag: IInterface;
        scriptRoot, fragmentsRoot: IInterface;
        i: integer;
        oldScriptName, newScriptName, curScriptName: string;
        doFragments: boolean;
        
    begin
        doFragments := false;
    
        vmad := ElementByName(e, 'VMAD - Virtual Machine Adapter');
        if(not assigned(vmad)) then begin
            exit;
        end;
        frags := ElementByName(vmad, 'Script Fragments');
        if(not assigned(frags)) then begin
            exit;
        end;
        
        //dumpElement(frags, '');
        
        scriptRoot := ElementByName(frags, 'Script');
        fragmentsRoot := ElementByName(frags, 'Fragments');
        
        if((not assigned(scriptRoot)) or (not assigned(fragmentsRoot))) then begin 
            exit;
        end;
        
        AddMessage('Processing: ' + FullPath(e));
        
        //dumpElement(scriptRoot, '');
  
        oldScriptName := GetElementEditValues(scriptRoot, 'scriptName');
        if(oldScriptName <> '') then begin
            newScriptName := migrateScript(oldScriptName);
            
            if(newScriptName <> '') then begin 
                AddMessage('Processing '+oldScriptName+' -> '+newScriptName);
                setElementEditValues(scriptRoot, 'scriptName', newScriptName);
                doFragments := true
            end;
        
        end else begin
            // sometimes, the scriptName is actually in the frags root
            oldScriptName := GetElementEditValues(frags, 'scriptName');
            newScriptName := migrateScript(oldScriptName);
            
            if(newScriptName <> '') then begin 
                AddMessage('Processing '+oldScriptName+' -> '+newScriptName);
                setElementEditValues(frags, 'scriptName', newScriptName);
                doFragments := true
            end;
        end;
        
        
        //AddMessage('FOO! '+oldScriptName+' bar');
        //newScriptName := migrateScript(oldScriptName);
        if(doFragments) then begin 
            // AddMessage('Processing '+oldScriptName+' -> '+newScriptName);
            // setElementEditValues(scriptRoot, 'scriptName', newScriptName);
            for i := 0 to ElementCount(fragmentsRoot)-1 do begin
                curFrag := ElementByIndex(fragmentsRoot, i);
                curScriptName := GetElementEditValues(curFrag, 'scriptName');
                if(curScriptName = oldScriptName) then begin
                    setElementEditValues(curFrag, 'scriptName', newScriptName);
                end;
                //AddMessage('===NAME: '+curScriptName);
                //dumpElement(curFrag, '')
            end;
            
        end;
    end;

    function Initialize: integer;
        var prefixName, curChar: string; 
            i: integer;
    begin
        if not InputQuery('Enter', 'Your namespace', prefixName) then begin
            Result := 1;
            exit;
        end;
        
        if length(prefixName) = 0 then begin
            AddMessage('No namespace given');
            Result := 1;
            exit;
        end;
        
        for i:=1 to length(prefixName) do begin
            curChar := copy(prefixName, i, 1);
            if(curChar = ' ') then begin
                AddMessage('The namespace cannot contain spaces!');
                result := 1;
                exit;
            end;
        end;
        
        AddMessage('Selected namespace: '+prefixName);
        SelectedNamespace := prefixName;
        initCompiler();
        Result := 0;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        sig: string;
    begin
        Result := 0;
        // I have no idea which signatures have scripts, so...
        processFragments(e);
    end;


end.
