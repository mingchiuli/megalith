package org.chiu.megalith.manage.listener;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.blog.config.EvictCacheRabbitConfig;
import org.chiu.megalith.manage.event.BlogOperateEvent;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.constant.BlogOperateEnum;
import org.chiu.megalith.infra.constant.BlogOperateMessage;
import org.chiu.megalith.search.config.ElasticSearchRabbitConfig;
import org.springframework.amqp.rabbit.connection.CorrelationData;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.context.event.EventListener;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

@Component
@RequiredArgsConstructor
public class BlogOperateEventListener {

    private final RabbitTemplate rabbitTemplate;

    private final StringRedisTemplate redisTemplate;

    @EventListener
    @Async("commonExecutor")
    public void process(BlogOperateEvent event) {
        BlogOperateMessage messageBody = event.getBlogOperateMessage();
        BlogOperateEnum typeEnum = messageBody.getTypeEnum();
        String name = typeEnum.name();
        Long blogId = messageBody.getBlogId();
        String key = name + "_" + blogId;

        var correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                key,
                30,
                TimeUnit.MINUTES);
        rabbitTemplate.convertAndSend(ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                messageBody,
                correlationData);

        rabbitTemplate.convertAndSend(EvictCacheRabbitConfig.CACHE_BLOG_EVICT_EXCHANGE,
                EvictCacheRabbitConfig.CACHE_BLOG_EVICT_BINDING_KEY,
                messageBody);
    }
}
