{
    Recompiles any scripts or fragments in the selected records.
}
unit RecompileScripts;
    
    uses praCompiler;
    
    var
        scriptsDone: TSTringList;

    procedure processScriptName(scriptName: string);
    var
        sourcePath: string;
        destDir: string;
    begin
        if(scriptsDone.indexOf(scriptName) >= 0) then begin
            exit;
        end;
        scriptsDone.add(scriptName);
        AddMessage('=================');
        AddMessage('Recompiling: '+scriptName);
        compileScriptByName(scriptName);
    end;
    
    
    procedure processScriptElem(script: IInterface);
    var
        curScriptName: string;
    begin
        curScriptName := GetElementEditValues(script, 'scriptName');
        if(curScriptName <> '') then begin 
            processScriptName(curScriptName);
        end;
    end;
  
    function Initialize: integer;
    begin
        initCompiler();
        scriptsDone := TSTringList.create;
        Result := 0;
    end;

    procedure processScripts(e: IInterface);
    var
        vmad, scriptList, frags, aliases: IInterface;
        curScript, curAlias: IInterface;
        i, j: integer;
        
        oldScriptName: string;
    begin
        vmad := ElementByName(e, 'VMAD - Virtual Machine Adapter');
        if(not assigned(vmad)) then begin
            exit;
        end;
        
        // scripts
        scriptList := ElementByName(vmad, 'Scripts');
        if(assigned(scriptList)) then begin
            
            for i := 0 to ElementCount(scriptList)-1 do begin
                curScript := ElementByIndex(scriptList, i);
                processScriptElem(curScript);
            end;
            
        end;
        
        // fragments
        frags := ElementByName(vmad, 'Script Fragments');
        if(assigned(frags)) then begin 
            curScript := ElementByName(frags, 'Script');
            if(assigned(curScript)) then begin 
                processScriptElem(curScript);
            end;
            
            // sometimes frags itself has a script
            processScriptElem(frags);
        end;
        
        // quest aliases
        aliases := ElementByName(vmad, 'Aliases');
        if(assigned(aliases)) then begin 
            for i := 0 to ElementCount(aliases)-1 do begin
                curAlias := ElementByIndex(aliases, i);
                scriptList := ElementByName(curAlias, 'Alias Scripts');
                for j := 0 to ElementCount(scriptList)-1 do begin
                    curScript := ElementByIndex(scriptList, j);
                    processScriptElem(curScript);
                end;
            end;
        end;
        
        
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    begin
        Result := 0;

        processScripts(e);
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        scriptsDone.free();
        Result := 0;
    end;


end.