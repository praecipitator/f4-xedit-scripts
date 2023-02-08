{
    Move deleted or disabled refs to a layer
    TODO make better
}
unit userscript;
    uses praUtil;
    const 
        layerName = 'praLibraryDeleted';
        
    var
        layerForm: IInterface;
        
    function isSomehowDeleted(e: IInterface): boolean;
    begin
        Result := (GetIsDeleted(e) or GetIsInitiallyDisabled(e));
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        
        layerForm := GetFormByEdid(layerName);
        if(not assigned(layerForm)) then begin
            AddMessage('Didn''t find layer: '+layerName);
            Result := 1;
        end;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    begin
        Result := 0;

        if(not isReferenceSignature(Signature(e))) then begin
            exit;
        end;
        // comment this out if you don't want those messages
        AddMessage('Processing: ' + FullPath(e));
        if(isSomehowDeleted(e)) then begin
            setPathLinksTo(e, 'XLYR', layerForm);
        end;

        // processing code goes here

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
    end;

end.