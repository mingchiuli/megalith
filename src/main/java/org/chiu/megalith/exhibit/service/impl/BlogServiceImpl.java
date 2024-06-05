package org.chiu.megalith.exhibit.service.impl;

import org.chiu.megalith.exhibit.convertor.BlogDescriptionVoConvertor;
import org.chiu.megalith.exhibit.convertor.BlogExhibitVoConvertor;
import org.chiu.megalith.exhibit.convertor.BlogHotReadVoConvertor;
import org.chiu.megalith.exhibit.convertor.VisitStatisticsVoConvertor;
import org.chiu.megalith.exhibit.dto.BlogDescriptionDto;
import org.chiu.megalith.exhibit.dto.BlogExhibitDto;
import org.chiu.megalith.exhibit.vo.BlogDescriptionVo;
import org.chiu.megalith.exhibit.vo.BlogExhibitVo;
import org.chiu.megalith.exhibit.vo.BlogHotReadVo;
import org.chiu.megalith.exhibit.vo.VisitStatisticsVo;
import org.chiu.megalith.exhibit.wrapper.BlogWrapper;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.security.utils.SecurityUtils;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.exhibit.service.BlogService;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;

import org.chiu.megalith.blog.repository.BlogRepository;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.util.ResourceUtils;
import org.springframework.util.StringUtils;

import jakarta.annotation.PostConstruct;

import java.nio.charset.StandardCharsets;
import java.util.*;

import static org.chiu.megalith.infra.lang.Const.*;
import static org.chiu.megalith.infra.lang.ExceptionMessage.*;


/**
 * @author mingchiuli
 * @create 2022-11-27 2:10 pm
 */
@Service
@RequiredArgsConstructor
public class BlogServiceImpl implements BlogService {

    private final BlogRepository blogRepository;

    private final StringRedisTemplate redisTemplate;

    private final BlogWrapper blogWrapper;

    private final SecurityUtils securityUtils;

    private final ResourceLoader resourceLoader;

    private String visitScript;

    private String countYearsScript;

    @PostConstruct
    @SneakyThrows
    private void init() {
        Resource visitResource = resourceLoader.getResource(ResourceUtils.CLASSPATH_URL_PREFIX + "script/visit.lua");
        Resource countYearsResource = resourceLoader.getResource(ResourceUtils.CLASSPATH_URL_PREFIX + "script/count-years.lua");
        visitScript = visitResource.getContentAsString(StandardCharsets.UTF_8);
        countYearsScript = countYearsResource.getContentAsString(StandardCharsets.UTF_8);
    }

    @Override
    public PageAdapter<BlogDescriptionVo> findPage(Integer currentPage, Integer year) {
        PageAdapter<BlogDescriptionDto> dtoPageAdapter = blogWrapper.findPage(currentPage, year);
        return BlogDescriptionVoConvertor.convert(dtoPageAdapter);
    }

    @Override
    public Boolean checkToken(Long blogId, String token) {
        token = token.trim();
        String password = redisTemplate.opsForValue().get(READ_TOKEN.getInfo() + blogId);
        if (StringUtils.hasLength(token) && StringUtils.hasLength(password)) {
            return password.equals(token);
        }
        return false;
    }

    @Override
    public Integer getBlogStatus(Authentication authentication, Long blogId) {
        Integer status = blogWrapper.findStatusById(blogId);

        if (StatusEnum.NORMAL.getCode().equals(status)) {
            return status;
        }

        if (Boolean.TRUE.equals(authentication instanceof AnonymousAuthenticationToken)) {
            return StatusEnum.HIDE.getCode();
        }

        if (Boolean.TRUE.equals(securityUtils.isAdmin(SecurityUtils.getLoginRole()))) {
            return StatusEnum.NORMAL.getCode();
        }

        String userId = authentication.getName();

        BlogEntity blog = blogRepository.findById(blogId)
                .orElseThrow(() -> new MissException(NO_FOUND));
        Long id = blog.getUserId();
        return Objects.equals(id, Long.valueOf(userId)) ?
                StatusEnum.NORMAL.getCode() :
                StatusEnum.HIDE.getCode();
    }

    @Override
    public BlogExhibitVo getLockedBlog(Long blogId, String token) {
        boolean valid = checkToken(blogId, token);
        if (valid) {
            blogWrapper.setReadCount(blogId);
            BlogExhibitDto blogExhibitDto = blogWrapper.findById(blogId);
            return BlogExhibitVoConvertor.convert(blogExhibitDto);
        }
        throw new BadCredentialsException(TOKEN_INVALID.getMsg());
    }

    @Override
    public List<Integer> searchYears() {
        Long count = Optional
                .ofNullable(
                        redisTemplate.execute(RedisScript.of(countYearsScript, Long.class), 
                                List.of(BLOOM_FILTER_YEARS.getInfo())))
                .orElse(0L);
        int start = 2021;
        int end = Math.max(start + count.intValue() - 1, start);
        var years = new ArrayList<Integer>(end - start + 1);
        for (int year = start; year <= end; year++) {
            years.add(year);
        }
        if (years.size() == 1) {
            years.add(start);
        }
        return years;
    }

    @Override
    @SuppressWarnings("unchecked")
    public VisitStatisticsVo getVisitStatistics() {
        List<Long> list = Optional.ofNullable(redisTemplate.execute(RedisScript.of(visitScript,List.class),
                        List.of(DAY_VISIT.getInfo(), WEEK_VISIT.getInfo(), MONTH_VISIT.getInfo(), YEAR_VISIT.getInfo())))
                .orElseGet(ArrayList::new);

        return VisitStatisticsVoConvertor.convert(list);
    }

    @Override
    public List<BlogHotReadVo> getScoreBlogs() {
        Set<ZSetOperations.TypedTuple<String>> set = redisTemplate.opsForZSet()
                .reverseRangeWithScores(HOT_READ.getInfo(), 0, 4);

        List<Long> ids = Optional.ofNullable(set).orElseGet(LinkedHashSet::new).stream()
                .map(item -> Long.valueOf(Optional.ofNullable(item.getValue()).orElse("0")))
                .toList();

        List<BlogEntity> blogs = blogRepository.findAllById(ids);

        return BlogHotReadVoConvertor.convert(blogs, set);
    }

    @Override
    public BlogExhibitVo getBlogDetail(Authentication authentication, Long id) {

        BlogExhibitDto blogExhibitDto = blogWrapper.findById(id);
        Integer status = blogWrapper.findStatusById(id);

        if (StatusEnum.NORMAL.getCode().equals(status) || Boolean.TRUE.equals(securityUtils.isAdmin(SecurityUtils.getLoginRole()))) {
            blogWrapper.setReadCount(id);
            return BlogExhibitVoConvertor.convert(blogExhibitDto);
        }

        if (authentication instanceof AnonymousAuthenticationToken) {
            throw new BadCredentialsException(AUTH_EXCEPTION.getMsg());
        }

        Long userId = Long.valueOf(authentication.getName());

        if (userId.equals(blogExhibitDto.getUserId())) {
            blogWrapper.setReadCount(id);
            return BlogExhibitVoConvertor.convert(blogExhibitDto);
        }

        throw new BadCredentialsException(AUTH_EXCEPTION.getMsg());
    }

    @Override
    public List<Integer> getYears() {
        return blogRepository.getYears();
    }


    @Override
    public Long count() {
        return blogRepository.count();
    }

    @Override
    public Long getCountByYear(Integer year) {
        return blogWrapper.getCountByYear(year);
    }

    @Override
    public List<Long> findIds(Pageable pageRequest) {
        return blogRepository.findIds(pageRequest);
    }
}
