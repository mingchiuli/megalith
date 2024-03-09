package org.chiu.megalith.manage.service.impl;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.manage.entity.AuthorityEntity;
import org.chiu.megalith.manage.repository.AuthorityRepository;
import org.chiu.megalith.manage.req.AuthorityEntityReq;
import org.chiu.megalith.manage.service.AuthorityService;
import org.chiu.megalith.manage.vo.AuthorityVo;
import org.chiu.megalith.manage.wrapper.AuthorityWrapper;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.chiu.megalith.manage.convertor.AuthorityVoConvertor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;

import static org.chiu.megalith.infra.lang.ExceptionMessage.NO_FOUND;

@Service
@RequiredArgsConstructor
public class AuthorityServiceImpl implements AuthorityService {

    private final AuthorityRepository authorityRepository;

    private final AuthorityWrapper authorityWrapper;

    @Override
    public List<AuthorityVo> findAll() {
        List<AuthorityEntity> authorityEntities = authorityRepository.findAll();
        return AuthorityVoConvertor.convert(authorityEntities);
    }

    @Override
    public AuthorityVo findById(Long id) {
        AuthorityEntity authorityEntity = authorityRepository.findById(id)
                .orElseThrow(() -> new MissException(NO_FOUND));
        return AuthorityVoConvertor.convert(authorityEntity);
    }

    @Override
    public void saveOrUpdate(AuthorityEntityReq req) {

        Long id = req.getId();
        AuthorityEntity authorityEntity;
        var now = LocalDateTime.now();

        if (Objects.nonNull(id)) {
            authorityEntity = authorityRepository.findById(id)
                    .orElseThrow(() -> new MissException(NO_FOUND));
            authorityEntity.setUpdated(now);
        } else {
            authorityEntity = AuthorityEntity.builder()
                    .created(now)
                    .updated(now)
                    .build();
        }

        BeanUtils.copyProperties(req, authorityEntity);
        authorityWrapper.save(authorityEntity);
    }

    @Override
    public void deleteAuthorities(List<Long> ids) {
        authorityWrapper.deleteAllById(ids);
    }
}
