{
    Processes object references, if it's a Mac piece with a material swap,
    it will try to replace it with an equivalent object from SS2.

    For example, if base object's EDID is 'MacBaseBlockAMed01', and it has the matswap 'MachineKitGray01',
    it will try to replace it with the object 'kgSIM_PBP_MacBaseBlockAMed01_Gray'
}
unit userscript;
    uses praUtil;
    var
        lookupCache: TStringList;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        lookupCache := TStringList.create;
        Result := 0;
    end;

    function getColorSuffixForMswp(mswp: IInterface): IInterface;
    var
        edid: string;
    begin
        Result := '';
        edid := EditorID(mswp);
        //kgSIM_PBP_MacBaseBlockAMed01_Black MachineKitBlack01
        //kgSIM_PBP_MacBaseBlockAMed01_Blue MachineKitBlue01
        //kgSIM_PBP_MacBaseBlockAMed01_ExtraLtBlue MachineKitBlueLight02
        //kgSIM_PBP_MacBaseBlockAMed01_Gray  MachineKitGray01
        //kgSIM_PBP_MacBaseBlockAMed01_Green MachineKitGreen01
        //kgSIM_PBP_MacBaseBlockAMed01_LtBlue MachineKitBlueLight01
        //kgSIM_PBP_MacBaseBlockAMed01_LtGreen MachineKitGreenLight01
        //kgSIM_PBP_MacBaseBlockAMed01_Red MachineKitRed01
        //kgSIM_PBP_MacBaseBlockAMed01_White MachineKitWhite01
        if(edid = 'MachineKitBlack01') then begin
            Result := '_Black';
            exit;
        end;

        if(edid = 'MachineKitBlue01') then begin
            Result := '_Blue';
            exit;
        end;

        if(edid = 'MachineKitBlueLight02') then begin
            Result := '_ExtraLtBlue';
            exit;
        end;

        if(edid = 'MachineKitGray01') then begin
            Result := '_Gray';
            exit;
        end;

        if(edid = 'MachineKitGreen01') then begin
            Result := '_Green';
            exit;
        end;

        if(edid = 'MachineKitBlueLight01') then begin
            Result := '_LtBlue';
            exit;
        end;

        if(edid = 'MachineKitGreenLight01') then begin
            Result := '_LtGreen';
            exit;
        end;

        if(edid = 'MachineKitRed01') then begin
            Result := '_Red';
            exit;
        end;

        if(edid = 'MachineKitWhite01') then begin
            Result := '_White';
            exit;
        end;

        // pipes
        if(edid = 'IndPipeBlueGreen') then begin
            Result := '_BlueGreen';
            exit;
        end;

        if(edid = 'IndPipeBronze') then begin
            Result := '_Bronze';
            exit;
        end;

        if(edid = 'IndPipeRed') then begin
            Result := '_Red';
            exit;
        end;

        if(edid = 'IndPipeWhite') then begin
            Result := '_White';
            exit;
        end;

    end;

    function getFormByEdidCached(edid: string): IInterface;
    var
        i: integer;
    begin
        i := lookupCache.indexOf(edid);
        if(i >= 0) then begin
            Result := ObjectToElement(lookupCache.Objects[i]);
            exit;
        end;

        AddMessage('Trying to find '+edid);
        Result := GetFormByEdid(edid);
        if(assigned(Result)) then begin
            lookupCache.addObject(edid, Result);
        end;
    end;

    procedure processRef(ref: IInterface; base: IInterface; mswp: IInterface);
    var
        suffix, newEdid: string;
        newBase: IInterface;
    begin
        suffix := getColorSuffixForMswp(mswp);
        if(suffix = '') then exit;

        newEdid := 'kgSIM_PBP_' + EditorID(base) + suffix;

        newBase := getFormByEdidCached(newEdid);
        if(assigned(newBase)) then begin
            setPathLinksTo(ref, 'NAME', newBase);
            removeElement(ref, 'XMSP');
        end;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        base, matswap: IInterface;
    begin
        Result := 0;
        if(Signature(e) <> 'REFR') then begin
            exit;
        end;
        base := pathLinksTo(e, 'NAME');
        matswap := pathLinksTo(e, 'XMSP');

        if(not assigned(matswap)) or (Signature(base) <> 'STAT') then begin
            exit;
        end;

        // comment this out if you don't want those messages
        AddMessage('Processing: ' + FullPath(e));
        processRef(e, base, matswap);
        // processing code goes here

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        lookupCache.free();
        Result := 0;
    end;

end.