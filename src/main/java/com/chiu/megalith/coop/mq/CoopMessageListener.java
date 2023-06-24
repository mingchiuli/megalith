package com.chiu.megalith.coop.mq;

import com.chiu.megalith.coop.dto.BaseDto;
import com.chiu.megalith.infra.utils.SpringUtils;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class CoopMessageListener {

    private static class CacheHandlers {
        private static final Map<String, BaseCoopHandler> cacheHandlers = SpringUtils.getHandlers(BaseCoopHandler.class);
    }

    @SuppressWarnings("unused")
    //	public static final String ORIGINAL_DEFAULT_LISTENER_METHOD = "handleMessage";
    public void handleMessage(BaseDto msg) {
        for (BaseCoopHandler handler : CacheHandlers.cacheHandlers.values()) {
            if (handler.supports(msg)) {
                handler.handle(msg);
                break;
            }
        }
    }
}
