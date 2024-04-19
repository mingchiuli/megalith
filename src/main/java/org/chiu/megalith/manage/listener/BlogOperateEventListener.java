package org.chiu.megalith.manage.listener;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.entity.BlogEntity;
import org.chiu.megalith.manage.event.BlogOperateEvent;
import org.chiu.megalith.manage.listener.cache.BlogCacheEvictHandler;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.chiu.megalith.infra.config.CacheEvictRabbitConfig;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.search.BlogIndexEnum;
import org.chiu.megalith.infra.search.BlogSearchIndexMessage;
import org.chiu.megalith.search.config.ElasticSearchRabbitConfig;
import org.springframework.amqp.rabbit.connection.CorrelationData;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.context.event.EventListener;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

@Component
@RequiredArgsConstructor
public class BlogOperateEventListener {

    private final RabbitTemplate rabbitTemplate;

    private final StringRedisTemplate redisTemplate;

    private final List<BlogCacheEvictHandler> blogCacheEvictHandlers;

    private final BlogRepository blogRepository;

    @EventListener
    @Async("commonExecutor")
    public void process(BlogOperateEvent event) {
        BlogSearchIndexMessage messageBody = event.getBlogSearchIndexMessage();
        BlogIndexEnum typeEnum = messageBody.getTypeEnum();
        String name = typeEnum.name();
        Long blogId = messageBody.getBlogId();
        Integer year = messageBody.getYear();
        String key = name + "_" + blogId;

        Set<String> keys = null;

        BlogEntity blogEntity = blogRepository.findById(blogId)
                .orElseGet(() -> BlogEntity.builder()
                        .id(blogId)
                        .created(LocalDateTime.of(year, 1,1,1,1,1, 1))
                        .build());

        for (BlogCacheEvictHandler handler : blogCacheEvictHandlers) {
            if (handler.match(typeEnum)) {
                keys = handler.handle(messageBody, blogEntity);
                break;
            }
        }

        if (!CollectionUtils.isEmpty(keys)) {
            rabbitTemplate.convertAndSend(CacheEvictRabbitConfig.CACHE_EVICT_FANOUT_EXCHANGE, "", keys);
        }

        var correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                key,
                30,
                TimeUnit.MINUTES);

        rabbitTemplate.convertAndSend(ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                messageBody,
                correlationData);
    }
}
