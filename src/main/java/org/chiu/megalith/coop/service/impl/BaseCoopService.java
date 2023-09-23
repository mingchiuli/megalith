package org.chiu.megalith.coop.service.impl;

import org.chiu.megalith.coop.config.CoopRabbitConfig;
import org.chiu.megalith.coop.dto.BaseTransferDto;
import org.chiu.megalith.coop.dto.UserEntityDto;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.utils.JsonUtils;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.util.*;


public abstract class BaseCoopService {

    protected StringRedisTemplate redisTemplate;

    protected JsonUtils jsonUtils;

    protected RabbitTemplate rabbitTemplate;

    public BaseCoopService(StringRedisTemplate redisTemplate, JsonUtils jsonUtils, RabbitTemplate rabbitTemplate) {
        this.redisTemplate = redisTemplate;
        this.jsonUtils = jsonUtils;
        this.rabbitTemplate = rabbitTemplate;
    }

    protected void sendToOtherUsers(BaseTransferDto dto) {
        Long blogId = dto.getBlogId();
        Long userId = dto.getFromId();

        HashOperations<String, String, String> operations = redisTemplate.opsForHash();
        Set<String> nodeMarkSet = new HashSet<>();
        Map<String, List<Long>> idMarkMap = new HashMap<>();
        operations.entries(Const.COOP_PREFIX.getInfo() + blogId)
                .forEach((key, value) -> {
                    UserEntityDto userVo = jsonUtils.readValue(value, UserEntityDto.class);
                    String nodeMark = userVo.getNodeMark();
                    List<Long> idList = idMarkMap.computeIfAbsent(nodeMark, k -> new ArrayList<>());
                    idList.add(userVo.getId());
                    if (!Objects.equals(Const.BLOG_CONTENT.getInfo(), key)
                            && !Objects.equals(userVo.getId(), userId)) {
                        nodeMarkSet.add(nodeMark);
                    }
                });

        nodeMarkSet.forEach(mark -> {
            List<Long> idList = idMarkMap.get(mark);
            dto.setToId(idList);
            rabbitTemplate.convertAndSend(CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                    CoopRabbitConfig.WS_BINDING_KEY + mark,
                    dto);
        });

    }
}
