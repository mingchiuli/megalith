package org.chiu.megalith.blog.cache.handler;

import com.rabbitmq.client.Channel;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.chiu.megalith.blog.config.CacheBlogEvictRabbitConfig;
import org.chiu.megalith.manage.entity.BlogEntity;
import org.chiu.megalith.infra.constant.BlogOperateEnum;
import org.chiu.megalith.infra.constant.BlogOperateMessage;
import org.chiu.megalith.manage.repository.BlogRepository;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.time.LocalDateTime;
import java.util.Set;

@Slf4j
public abstract sealed class BlogCacheEvictHandler permits
        CreateBlogCacheEvictHandler,
        DeleteBlogCacheEvictHandler,
        UpdateBlogCacheEvictHandler {

    protected final StringRedisTemplate redisTemplate;

    protected final BlogRepository blogRepository;

    protected final RabbitTemplate rabbitTemplate;

    protected BlogCacheEvictHandler(StringRedisTemplate redisTemplate,
                                    BlogRepository blogRepository,
                                    RabbitTemplate rabbitTemplate) {
        this.redisTemplate = redisTemplate;
        this.blogRepository = blogRepository;
        this.rabbitTemplate = rabbitTemplate;
    }

    public abstract boolean supports(BlogOperateEnum blogOperateEnum);

    protected abstract Set<String> redisProcess(BlogEntity blog);


    @SneakyThrows
    public void handle(BlogOperateMessage message, Channel channel, Message msg) {
        long deliveryTag = msg.getMessageProperties().getDeliveryTag();
        try {
            Long blogId = message.getBlogId();
            Integer year = message.getYear();
            BlogEntity blogEntity = blogRepository.findById(blogId)
                    .orElseGet(() -> BlogEntity.builder()
                            .id(blogId)
                            .created(LocalDateTime.of(year, 1, 1, 1, 1, 1))
                            .build());

            Set<String> keys = redisProcess(blogEntity);
            rabbitTemplate.convertAndSend(CacheBlogEvictRabbitConfig.CACHE_BLOG_EVICT_FANOUT_EXCHANGE, "", keys);

            //手动签收消息
            //false代表不是批量签收模式
            channel.basicAck(deliveryTag, false);
        } catch (Exception e) {
            log.error("consume failure", e);
            channel.basicNack(deliveryTag, false, true);
        }
    }
}
