package org.chiu.megalith.infra.listener;

import java.util.Set;

import org.springframework.stereotype.Component;
import com.rabbitmq.client.Channel;
import org.springframework.amqp.core.Message;
import com.github.benmanes.caffeine.cache.Cache;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

@Component
@RequiredArgsConstructor
@Slf4j
public class CacheMessageListener {

    private final Cache<String, Object> localCache;

    @SneakyThrows
    public void handleMessage(Set<String> keys, Channel channel, Message msg) {
        long deliveryTag = msg.getMessageProperties().getDeliveryTag();
        try {
            localCache.invalidateAll(keys);
            channel.basicAck(deliveryTag, false);
        } catch(Exception e) {
            channel.basicNack(deliveryTag, false, true);
            log.error("consume failure", e);
        }
    }
}
