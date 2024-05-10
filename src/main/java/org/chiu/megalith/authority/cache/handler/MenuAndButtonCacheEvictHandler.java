package org.chiu.megalith.authority.cache.handler;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.chiu.megalith.infra.cache.CacheKeyGenerator;
import org.chiu.megalith.authority.cache.CacheEvictHandler;
import org.chiu.megalith.authority.entity.RoleEntity;
import org.chiu.megalith.authority.repository.RoleRepository;
import org.chiu.megalith.authority.wrapper.RoleMenuWrapper;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.*;


@RequiredArgsConstructor
@Component
public class MenuAndButtonCacheEvictHandler extends CacheEvictHandler {

    private final CacheKeyGenerator cacheKeyGenerator;

    private final RoleRepository roleRepository;

    @SneakyThrows
    @Override
    public Set<String> handle(Object[] args) {
        Object role = args[0];
        Object roleCode;

        switch (role) {
            case String ignored -> roleCode = role;
            case Long roleId -> {
                Optional<RoleEntity> optionalRole = roleRepository.findById(roleId);
                if (optionalRole.isPresent()) {
                    roleCode = optionalRole.get().getCode();
                } else {
                    return Collections.emptySet();
                }
            }
            case RoleEntity roleEntity -> roleCode = roleEntity.getCode();
            case null, default -> {
                return Collections.emptySet();
            }
        }

        Method method = RoleMenuWrapper.class.getMethod("getCurrentRoleNav", String.class);
        String key = cacheKeyGenerator.generateKey(method, roleCode);
        return Collections.singleton(key);
    }
}
