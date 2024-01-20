package org.chiu.megalith.blog.convertor;

import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.vo.BlogHotReadVo;
import org.chiu.megalith.infra.exception.MissException;
import org.springframework.data.redis.core.ZSetOperations;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import static org.chiu.megalith.infra.lang.ExceptionMessage.NO_FOUND;

public class BlogHotReadVoConvertor {

    public static List<BlogHotReadVo> convert(List<BlogEntity> blogs, Set<ZSetOperations.TypedTuple<String>> set) {

        List<BlogHotReadVo> items = Optional.ofNullable(set).orElseGet(LinkedHashSet::new).stream()
                .map(item -> BlogHotReadVo.builder()
                        .id(Long.valueOf(item.getValue()))
                        .readCount(item.getScore().longValue())
                        .build())
                .toList();

        items.forEach(item -> {
            String title = blogs.stream()
                    .filter(blog -> blog.getId().equals(item.getId()))
                    .findAny()
                    .orElseThrow(() -> new MissException(NO_FOUND))
                    .getTitle();
            item.setTitle(title);
        });

        return items;
    }
}
