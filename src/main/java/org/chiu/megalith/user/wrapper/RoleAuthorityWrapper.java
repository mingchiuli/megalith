package org.chiu.megalith.user.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.cache.Cache;
import org.chiu.megalith.user.cache.CacheEvict;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.user.cache.handler.AuthorityCacheEvictHandler;
import org.chiu.megalith.user.entity.AuthorityEntity;
import org.chiu.megalith.user.entity.RoleAuthorityEntity;
import org.chiu.megalith.user.entity.RoleEntity;
import org.chiu.megalith.user.repository.AuthorityRepository;
import org.chiu.megalith.user.repository.RoleAuthorityRepository;
import org.chiu.megalith.user.repository.RoleRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.chiu.megalith.infra.lang.StatusEnum.NORMAL;

@Component
@RequiredArgsConstructor
public class RoleAuthorityWrapper {

    private final RoleAuthorityRepository roleAuthorityRepository;

    private final AuthorityRepository authorityRepository;

    private final RoleRepository roleRepository;

    @Cache(prefix = Const.HOT_AUTHORITIES)
    public List<String> getAuthoritiesByRoleCode(String roleCode) {

        if ("REFRESH_TOKEN".equals(roleCode)) {
            return Collections.singletonList("token:refresh");
        }

        Optional<RoleEntity> roleEntityOptional = roleRepository.findByCodeAndStatus(roleCode, NORMAL.getCode());

        if (roleEntityOptional.isEmpty()) {
            return Collections.emptyList();
        }

        RoleEntity roleEntity = roleEntityOptional.get();

        List<Long> authorityIds = roleAuthorityRepository.findByRoleId(roleEntity.getId()).stream()
                .map(RoleAuthorityEntity::getAuthorityId)
                .toList();

        List<AuthorityEntity> authorities = authorityRepository.findAllById(authorityIds).stream()
                .filter(item -> NORMAL.getCode().equals(item.getStatus()))
                .toList();

        return authorities.stream()
                .map(AuthorityEntity::getCode)
                .toList();
    }

    @Transactional
    @CacheEvict(handler = { AuthorityCacheEvictHandler.class })
    public void saveAuthority(Long roleId, List<RoleAuthorityEntity> roleAuthorityEntities) {
        roleAuthorityRepository.deleteByRoleId(roleId);
        roleAuthorityRepository.saveAll(roleAuthorityEntities);
    }
}
