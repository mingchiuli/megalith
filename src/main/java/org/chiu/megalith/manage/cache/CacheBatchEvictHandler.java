package org.chiu.megalith.manage.cache;

import java.util.Set;

public interface CacheBatchEvictHandler {

    boolean match(String prefix);

    Set<String> handle(String prefix);
}
