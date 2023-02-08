{
    An attempt to implement some of the missing math unit functions for xEdit pascal.
    Algorithms taken from here: http://mathonweb.com/help_ebook/html/algorithms.htm
}
unit XeditSimpleMath;

    const
        MATH_PI = 3.1415926535897932;
        MATH_HALF_PI = MATH_PI / 2.0;
        MATH_QUARTER_PI = MATH_PI / 4.0;
        MATH_TWO_PI = 2.0 * MATH_PI;
        MATH_THREE_HALVES_PI = MATH_PI * 3.0/2.0;

    /////////////// HELPERS ///////////////

    {
        Returns true if `value1` is within `margin` of `value2`
    }
    function isWithin(value1, value2, margin: float): boolean;
    begin
        Result := AbsoluteValue(value1-value2) <= margin;
    end;

    {
        Returns the absolute value
    }
    function AbsoluteValue(val: float): float;
    begin
        if(val < 0) then begin
            Result := val * -1;
            exit;
        end;

        Result := val;
    end;

    {
        Normalizes an angle
    }
    function normalizeAngle(rad: float): float;
    begin
        while (rad < 0) do begin
            rad := rad + MATH_TWO_PI;
        end;

        while (rad > MATH_TWO_PI) do begin
            rad := rad - MATH_TWO_PI;
        end;

        Result := rad;
    end;

    /////////////// ACTUAL MATH FUNCTIONS ///////////////

    function DegToRad(degrees: float): float;
    begin
         Result := degrees / 180.0 * MATH_PI;
    end;

    function RadToDeg(rad: float): float;
    begin
         Result := rad * 180.0 / MATH_PI;
    end;


    // SIN
    function sin(x: float): float;
    begin
        Result := sinReal(x);
        // AddMessage('MATH: sin('+FloatToStr(x)+') = '+FloatToStr(Result));
    end;

    // source: http://mathonweb.com/help_ebook/html/algorithms.htm#sin
    function sinReal(x: float): float;
    begin
        x := normalizeAngle(x);

        if (x = 0) or (x = MATH_PI) or (x = MATH_TWO_PI) then begin
            Result := 0;
            exit;
        end;

        if(x = MATH_HALF_PI) then begin
            Result := 1;
        end;

        if(x = MATH_THREE_HALVES_PI) then begin
            Result := -1;
            exit;
        end;

        if(x > MATH_THREE_HALVES_PI) then begin
            // -sin(2pi-x)
            Result := sinApproximation(MATH_HALF_PI - x) * -1;
            exit;
        end;



        if(x > MATH_TWO_PI) then begin
            // -sin(x-pi)
            Result := sinApproximation(x-MATH_PI);
            exit;
        end;

        if(x > MATH_HALF_PI) then begin
            // sin(pi-x)
            Result :=  sinApproximation(MATH_PI-x);
            exit;
        end;



        Result := sinApproximation(x);

    end;


    function sinApproximation(x: float): float;
    begin
        if(x > MATH_QUARTER_PI) then begin
            Result := cosApproximation(MATH_HALF_PI-x);
            exit;
        end;
        Result := x - power(x, 3)/6 + power(x, 5)/120;
    end;

    function cos(x: float): float;
    begin
        Result := cosReal(x);
        // AddMessage('MATH: cos('+FloatToStr(x)+') = '+FloatToStr(Result));
    end;

    // COS
    function cosReal(x: float): float;
    begin
        x := normalizeAngle(x);

        if(x = MATH_HALF_PI) or (x = MATH_THREE_HALVES_PI) then begin
            Result := 0;
            exit;
        end;

        if(x = 0) or (x = MATH_TWO_PI) then begin
            Result := 1;
            exit;
        end;

        if(x = MATH_PI) then begin
            Result := -1;
            exit;
        end;

        if(x > MATH_THREE_HALVES_PI) then begin
            // cos(2pi-x)
            Result := cosApproximation(MATH_TWO_PI - x);
            exit;
        end;

        if(x > MATH_PI) then begin
            // -cos(x-pi)
            Result := -1 * cosApproximation(x-MATH_PI);
            exit;
        end;

        if(x > MATH_HALF_PI) then begin
            // -cos(pi-x)
            Result := -1 * cosApproximation(MATH_PI-x);
        end;
        Result := cosApproximation(x);
    end;

    function cosApproximation(x: float): float;
    begin
        if(x > MATH_QUARTER_PI) then begin
            Result := sinApproximation(MATH_HALF_PI-x);
            exit;
        end;
        Result := 1 - x*x/2 + power(x, 4)/24 + power(x, 6)/720;
    end;

    // TAN
    function tan(x: float): float;
    begin
        Result := tanReal(x);
        // AddMessage('MATH: tan('+FloatToStr(x)+') = '+FloatToStr(Result));
    end;

    function tanReal(x: float): float;
    begin
        // tan needs it's own normalisation
        while (x < 0) do begin
            x := x + MATH_PI;
        end;

        while (x > MATH_PI) do begin
            x := x - MATH_PI;
        end;

        if(x = 0) or (x = MATH_PI) then begin
            Result := 0;
            exit;
        end;

        if(x = MATH_HALF_PI) then begin
            // no idea if I can simulate NaN/Infinity here... oh well
            Result := 0.0/0.0;
            exit;
        end;

        if(x > MATH_HALF_PI) then begin
            // -tan(pi-x)
            Result := tanApproximation(MATH_PI-x) * -1;
            exit;
        end;

        Result := tanApproximation(x);

    end;

    function tanApproximation(x: float): float;
    var
        tanHalfX: float;
    begin
        if(x > MATH_PI / 4.0) then begin
            Result := 1.0/tanApproximation(MATH_PI - x);
        end;

        if(x > MATH_PI / 8.0) then begin
            tanHalfX := tanApproximation(x / 2.0);
            Result := 2*tanHalfX / (1 - tanHalfX*tanHalfX);
            exit;
        end;

        Result := x + power(x, 3)/3 + 2*power(x,5)/15 + 17*power(x,7)/315;
    end;

    // ASIN
    function asin(x: float): float;
    begin
        Result := asinReal(x);
        // AddMessage('MATH: asin('+FloatToStr(x)+') = '+FloatToStr(Result));
    end;

    function asinReal(x: float): float;
    begin
        if (x = 1) then begin
            Result := MATH_HALF_PI;
            exit;
        end;

        if (x = -1) then begin
            Result := MATH_THREE_HALVES_PI;
            exit;
        end;

        Result := atan( x / sqrt(1-power(x, 2))  );
    end;

    function arcsin(x: float): float;
    begin
        Result := asin(x);
    end;

    // ACOS
    function acos(x: float): float;
    begin
        Result := acosReal(x);
        // AddMessage('MATH: acos('+FloatToStr(x)+') = '+FloatToStr(Result));
    end;

    function acosReal(x: float): float;
    begin
        if(x = 0) then begin
            Result := MATH_HALF_PI;
            exit;
        end;
        Result := atan( sqrt(1-power(x,2)) / x );
    end;

    function arccos(x: float): float;
    begin
        Result := acos(x);
    end;


    // ATAN
    function atan(x: float): float;
    begin
        Result := atanReal(x);
        // AddMessage('MATH: atan('+FloatToStr(x)+') = '+FloatToStr(Result));
    end;

    function atanReal(x: float): float;
    var
        sqrt3: float;
    begin
        // atan(-x) = -atan(x)
        if(x < 0) then begin
            Result := -1 * atan(x * -1);
            exit;
        end;

        // atan(x) = pi/2 - atan(1/x)
        if(x > 1) then begin
            Result := MATH_HALF_PI - atan( 1.0 / x );
            exit;
        end;

        sqrt3 := sqrt(3.0);

        if(x > (2.0 - sqrt3)) then begin
            Result := MATH_PI / 6.0 + atan( (sqrt3*x - 1) / (sqrt3 + x) );
            exit;
        end;

        Result := x - power(x, 3)/3.0 + power(x, 5)/5.0;
    end;

    function arctan(x: float): float;
    begin
        Result := atan(x);
    end;

    // special ones



    function sinDeg(x: float): float;
    begin
        Result := sin(DegToRad(x));
    end;

    function cosDeg(x: float): float;
    begin
        Result := cos(DegToRad(x));
    end;

    function tanDeg(x: float): float;
    begin
        Result := tan(DegToRad(x));
    end;

    function asinDeg(x: float): float;
    begin
        Result := RadToDeg(arcsin(x));
    end;

    function acosDeg(x: float): float;
    begin
        Result := RadToDeg(arccos(x));
    end;

    function atanDeg(x: float): float;
    begin
        Result := RadToDeg(arctan(x));
    end;
end.