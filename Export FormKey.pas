{
    Exports 6-digit FormIDs and filenames, to be used as FormKey strings in Mutagen
}
unit userscript;
    uses praUtil;
    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        rec: IInterface;
        srcFile: IInterface;
        theFormId: cardinal;
    begin
        Result := 0;
        rec := MasterOrSelf(e);
        
        srcFile := GetFile(e);
        theFormId := getLocalFormId(srcFile, FormID(rec));

        // comment this out if you don't want those messages
        AddMessage(IntToHex(theFormId, 6)+':'+GetFileName(srcFile));

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
    end;

end.