package org.chiu.megalith.manage.cache;

import java.util.Set;

public interface CacheEvictHandler {

    boolean match(String prefix);

    Set<String> handle(String prefix);
}
