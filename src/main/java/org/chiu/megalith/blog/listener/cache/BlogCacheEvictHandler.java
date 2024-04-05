package org.chiu.megalith.blog.listener.cache;

import org.chiu.megalith.infra.search.BlogIndexEnum;
import org.chiu.megalith.infra.search.BlogSearchIndexMessage;

import java.util.Set;

public interface BlogCacheEvictHandler {

    boolean match(BlogIndexEnum blogIndexEnum);

    Set<String> handle(BlogSearchIndexMessage blogSearchIndexMessage);
}
