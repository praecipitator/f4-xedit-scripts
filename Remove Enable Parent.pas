{
    removes the enable parent ot what you run it on
}
unit userscript;
    uses praUtil;
    
    var
        targetFile: IInterface;
        targetFileName: string;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        targetFileName := 'FLH Settlement Update.esp';
        targetFile := FindFile(targetFileName);
        if(not assigned(targetFile)) then begin
            AddMessage('fail');
            Result := 1;
        end;
    end;

    procedure processEnableParent(e: IInterface);
    var
        i: integer;
        enableParent, curRef: IInterface;
    begin
        for i := ReferencedByCount(e)-1 downto 0 do begin
           
            curRef := ReferencedByIndex(e, i);
            enableParent := pathLinksTo(curRef, 'XESP\Reference');
            if(IsSameForm(enableParent, e)) then begin
                AddMessage('Found '+IntToHex(FormID(curRef), 8)+'');
                if(not isSameFile(targetFile, getFile(curRef))) then begin
                    curRef := getOrCreateElementOverride(curRef, targetFile);
                end;
                RemoveElement(curRef, 'XESP');
            end;
        end;
    end;


    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    begin
        Result := 0;

        // comment this out if you don't want those messages
        AddMessage('Processing: ' + FullPath(e));
        processEnableParent(e);
        // processing code goes here

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
    end;

end.