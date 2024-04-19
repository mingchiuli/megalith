package org.chiu.megalith.infra.cache;

import java.util.Set;

public interface CacheBatchEvictHandler {

    boolean match(String prefix);

    Set<String> handle(String prefix);
}
