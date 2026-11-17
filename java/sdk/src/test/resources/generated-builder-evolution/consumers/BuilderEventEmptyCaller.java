package fixture;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.MatrixEmptyEvent;

public final class BuilderEventEmptyCaller {
    public static void main(String[] args) throws Exception {
        System.out.println(new ObjectMapper().writeValueAsString(MatrixEmptyEvent.MatrixEmptyEventData.builder().build()));
    }
}
