package org.chiu.megalith.blog.convertor;

import org.chiu.megalith.blog.entity.BlogEntity;
import org.springframework.util.StringUtils;

import java.util.Map;

import static org.chiu.megalith.blog.lang.MessageActionFieldEnum.*;

public class BlogEntityConvertor {

    public static BlogEntity convert(Map<String, String> entries) {
        String idStr = entries.get(ID.getMsg());
        return BlogEntity.builder()
                .id(StringUtils.hasLength(idStr) ?
                        Long.valueOf(idStr) :
                        null)
                .description(entries.get(DESCRIPTION.getMsg()))
                .title(entries.get(TITLE.getMsg()))
                .status(Integer.valueOf(entries.get(STATUS.getMsg())))
                .link(entries.get(LINK.getMsg()))
                .build();
    }
}
