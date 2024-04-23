package org.chiu.megalith.manage.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.cache.Cache;
import org.chiu.megalith.manage.cache.CacheEvict;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.manage.cache.handler.AuthorityCacheEvictHandler;
import org.chiu.megalith.manage.entity.AuthorityEntity;
import org.chiu.megalith.manage.entity.RoleAuthorityEntity;
import org.chiu.megalith.manage.entity.RoleEntity;
import org.chiu.megalith.manage.repository.AuthorityRepository;
import org.chiu.megalith.manage.repository.RoleAuthorityRepository;
import org.chiu.megalith.manage.repository.RoleRepository;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.chiu.megalith.infra.lang.ExceptionMessage.NO_AUTH;
import static org.chiu.megalith.infra.lang.ExceptionMessage.NO_FOUND;
import static org.chiu.megalith.infra.lang.StatusEnum.HIDE;
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

        RoleEntity roleEntity = roleRepository.findByCodeAndStatus(roleCode, NORMAL.getCode())
                .orElseThrow(() -> new MissException(NO_FOUND));

        Integer status = roleEntity.getStatus();
        if (HIDE.getCode().equals(status)) {
            throw new BadCredentialsException(NO_AUTH.getMsg());
        }

        List<RoleAuthorityEntity> authorityEntities = roleAuthorityRepository.findByRoleId(roleEntity.getId());
        Set<Long> authorityIds = authorityEntities.stream()
                .map(RoleAuthorityEntity::getAuthorityId)
                .collect(Collectors.toSet());

        List<AuthorityEntity> authorities = authorityRepository.findAllById(authorityIds);

        return authorities.stream()
                .map(AuthorityEntity::getCode)
                .toList();
    }

    @Transactional
    @CacheEvict(handler = { AuthorityCacheEvictHandler.class })
    public void saveAuthority(Long roleId, ArrayList<RoleAuthorityEntity> roleAuthorityEntities) {
        roleAuthorityRepository.deleteByRoleId(roleId);
        roleAuthorityRepository.saveAll(roleAuthorityEntities);
    }
}
