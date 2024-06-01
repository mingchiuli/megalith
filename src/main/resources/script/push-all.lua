redis.call('del', KEYS[1])
local paragraphList = cjson.decode(ARGV[1])
for i=1, #paragraphList do
  redis.call('hset', KEYS[1], 'para::' .. i, paragraphList[i])
end
redis.call('hset', KEYS[1], ARGV[2], ARGV[9])
redis.call('hset', KEYS[1], ARGV[3], ARGV[10])
redis.call('hset', KEYS[1], ARGV[4], ARGV[11])
redis.call('hset', KEYS[1], ARGV[5], ARGV[12])
redis.call('hset', KEYS[1], ARGV[6], ARGV[13])
redis.call('hset', KEYS[1], ARGV[7], ARGV[14])
redis.call('hset', KEYS[1], ARGV[8], ARGV[15])
redis.call('expire', KEYS[1], ARGV[16])