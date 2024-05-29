package org.chiu.megalith.user.cache;

import java.util.Set;

public abstract class CacheEvictHandler {

    public boolean match(Class<? extends CacheEvictHandler> clazz) {
        return clazz.equals(this.getClass());
    }

    public abstract Set<String> handle(Object[] args);
}
