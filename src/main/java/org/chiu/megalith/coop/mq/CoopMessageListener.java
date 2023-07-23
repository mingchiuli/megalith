package org.chiu.megalith.coop.mq;

import org.chiu.megalith.coop.dto.BaseDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class CoopMessageListener {

    private final List<BaseCoopHandler> cacheHandlers;

    //public static final String ORIGINAL_DEFAULT_LISTENER_METHOD = "handleMessage";
    public void handleMessage(BaseDto msg) {
        for (BaseCoopHandler handler : cacheHandlers) {
            if (handler.supports(msg)) {
                handler.handle(msg);
                break;
            }
        }
    }
}
