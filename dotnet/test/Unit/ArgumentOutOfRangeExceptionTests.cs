/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using Xunit;

namespace GitHub.Copilot.Test.Unit;

public class ArgumentOutOfRangeExceptionTests
{
    [Fact]
    public void ThrowIfNegative_Reports_Argument_Name_And_Value()
    {
        int value = -1;

        var error = Assert.Throws<ArgumentOutOfRangeException>(() =>
            ArgumentOutOfRangeException.ThrowIfNegative(value));

        Assert.Equal(nameof(value), error.ParamName);
        Assert.Equal(value, Assert.IsType<int>(error.ActualValue));
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    public void ThrowIfNegative_Allows_Nonnegative_Values(int value)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(value);
    }
}
