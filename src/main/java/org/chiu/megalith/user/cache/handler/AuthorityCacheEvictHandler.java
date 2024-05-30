package org.chiu.megalith.user.cache.handler;


import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.chiu.megalith.user.cache.CacheEvictHandler;
import org.chiu.megalith.infra.cache.CacheKeyGenerator;
import org.chiu.megalith.user.entity.RoleEntity;
import org.chiu.megalith.user.repository.RoleRepository;
import org.chiu.megalith.user.wrapper.RoleAuthorityWrapper;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;


@Component
@RequiredArgsConstructor
public class AuthorityCacheEvictHandler extends CacheEvictHandler {

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

        Method method = RoleAuthorityWrapper.class.getMethod("getAuthoritiesByRoleCode", String.class);
        String key = cacheKeyGenerator.generateKey(method, roleCode);
        return Collections.singleton(key);
    }

}
